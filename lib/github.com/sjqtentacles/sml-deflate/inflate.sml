(* inflate.sml -- RFC 1951 DEFLATE decompression.

   A straightforward, correctness-first implementation:
     - a bit reader that consumes the input LSB-first (per RFC 1951 3.1.1);
     - canonical Huffman decoding from a code-length list (3.2.2);
     - the fixed literal/length and distance trees (3.2.6);
     - dynamic trees from the encoded code-length sequence (3.2.7);
     - LZ77 length/distance back-references into a growing output buffer.

   Output is accumulated into a growing CharArray. We raise the internal
   exception `Bad` on any malformed structure and convert to NONE at the top. *)

structure Inflate :> INFLATE =
struct
  exception Bad

  (* ----- bit reader (LSB-first) ----- *)
  (* state: input string, byte position, current byte, #bits left in it *)
  type reader = { s : string, pos : int ref, cur : int ref, nbits : int ref }

  fun newReader s = { s = s, pos = ref 0, cur = ref 0, nbits = ref 0 }

  fun nextByte (r : reader) =
    let val p = !(#pos r) in
      if p >= String.size (#s r) then raise Bad
      else (#pos r := p + 1; Char.ord (String.sub (#s r, p)))
    end

  (* read a single bit *)
  fun getBit (r : reader) =
    if !(#nbits r) = 0
    then (#cur r := nextByte r; #nbits r := 8;
          let val b = !(#cur r) mod 2
          in #cur r := !(#cur r) div 2; #nbits r := 7; b end)
    else (let val b = !(#cur r) mod 2
          in #cur r := !(#cur r) div 2; #nbits r := !(#nbits r) - 1; b end)

  (* read n bits, LSB first, as an int *)
  fun getBits r n =
    let
      fun loop (i, acc, mult) =
        if i >= n then acc
        else loop (i + 1, acc + getBit r * mult, mult * 2)
    in
      loop (0, 0, 1)
    end

  (* discard remaining bits in the current byte (for stored blocks) *)
  fun alignByte (r : reader) = (#nbits r := 0)

  (* ----- Huffman tables -----
     Represent a code table as: for each code length, the list of symbols, plus
     a decode function. We build a canonical decoder following RFC 1951 3.2.2.
     We store, per length L (1..maxlen): first code value and the symbol array
     offset. Decoding reads bits MSB-into-code one at a time. *)

  (* Build a decoder from an array of code lengths (indexed by symbol).
     Returns a function reader -> symbol. *)
  fun buildDecoder (lengths : int vector) =
    let
      val n = Vector.length lengths
      val maxBits = Vector.foldl Int.max 0 lengths
      (* count of codes per length *)
      val blCount = Array.array (maxBits + 1, 0)
      val () = Vector.app
                 (fn len => if len > 0
                            then Array.update (blCount, len,
                                   Array.sub (blCount, len) + 1)
                            else ()) lengths
      (* first code for each length *)
      val nextCode = Array.array (maxBits + 1, 0)
      val () =
        let
          fun loop (bits, code) =
            if bits > maxBits then ()
            else
              let
                val code' = (code + Array.sub (blCount, bits - 1)) * 2
              in
                Array.update (nextCode, bits, code');
                loop (bits + 1, code')
              end
        in
          if maxBits >= 1 then loop (1, 0) else ()
        end
      (* assign a code to each symbol; build map (length, code) -> symbol.
         We store codes in a list keyed by length for sequential matching. *)
      val codeOf = Array.array (n, ~1)   (* code value per symbol, ~1 if none *)
      val () =
        let
          fun loop sym =
            if sym >= n then ()
            else
              let val len = Vector.sub (lengths, sym) in
                (if len <> 0 then
                   (Array.update (codeOf, sym, Array.sub (nextCode, len));
                    Array.update (nextCode, len, Array.sub (nextCode, len) + 1))
                 else ());
                loop (sym + 1)
              end
        in
          loop 0
        end
      (* decode: read bits MSB-first building up `code`, matching length L. *)
      fun decode r =
        let
          fun loop (len, code) =
            if len > maxBits then raise Bad
            else
              let
                val code' = code * 2 + getBit r
                val len' = len + 1
                (* find symbol with this length and code *)
                fun findSym sym =
                  if sym >= n then NONE
                  else if Vector.sub (lengths, sym) = len'
                          andalso Array.sub (codeOf, sym) = code'
                       then SOME sym
                       else findSym (sym + 1)
              in
                case findSym 0 of
                    SOME sym => sym
                  | NONE => loop (len', code')
              end
        in
          loop (0, 0)
        end
    in
      decode
    end

  (* length/distance base+extra tables (RFC 1951 3.2.5) *)
  val lenBase = Vector.fromList [3,4,5,6,7,8,9,10,11,13,15,17,19,23,27,31,35,43,51,59,
                  67,83,99,115,131,163,195,227,258]
  val lenExtra = Vector.fromList [0,0,0,0,0,0,0,0,1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4,5,5,5,5,0]
  val distBase = Vector.fromList [1,2,3,4,5,7,9,13,17,25,33,49,65,97,129,193,257,385,513,769,
                   1025,1537,2049,3073,4097,6145,8193,12289,16385,24577]
  val distExtra = Vector.fromList [0,0,0,0,1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9,10,10,11,11,
                    12,12,13,13]

  (* fixed Huffman code lengths (RFC 1951 3.2.6) *)
  fun fixedLitLengths () =
    Vector.tabulate (288, fn i =>
      if i <= 143 then 8
      else if i <= 255 then 9
      else if i <= 279 then 7
      else 8)
  fun fixedDistLengths () = Vector.tabulate (30, fn _ => 5)

  (* growable output buffer *)
  fun inflate input =
    let
      val r = newReader input
      val cap = ref 1024
      val buf = ref (CharArray.array (!cap, #"\000"))
      val len = ref 0
      fun ensure extra =
        if !len + extra <= !cap then ()
        else
          let
            val newCap = ref (!cap)
            val () = while !len + extra > !newCap do newCap := !newCap * 2
            val nb = CharArray.array (!newCap, #"\000")
          in
            CharArray.copy { src = !buf, dst = nb, di = 0 };
            buf := nb; cap := !newCap
          end
      fun putByte b =
        (ensure 1; CharArray.update (!buf, !len, Char.chr b); len := !len + 1)
      fun byteAt i = Char.ord (CharArray.sub (!buf, i))

      fun inflateBlock (litDec, distDec) =
        let
          fun loop () =
            let val sym = litDec r in
              if sym = 256 then ()           (* end of block *)
              else if sym < 256 then (putByte sym; loop ())
              else
                let
                  val li = sym - 257
                  val _ = if li >= Vector.length lenBase then raise Bad else ()
                  val length =
                    Vector.sub (lenBase, li) +
                    getBits r (Vector.sub (lenExtra, li))
                  val dsym = distDec r
                  val _ = if dsym >= Vector.length distBase then raise Bad else ()
                  val dist =
                    Vector.sub (distBase, dsym) +
                    getBits r (Vector.sub (distExtra, dsym))
                  val () = if dist > !len then raise Bad else ()
                  fun copy k =
                    if k >= length then ()
                    else (putByte (byteAt (!len - dist)); copy (k + 1))
                in
                  copy 0; loop ()
                end
            end
        in
          loop ()
        end

      fun storedBlock () =
        let
          val () = alignByte r
          val lenLo = nextByte r
          val lenHi = nextByte r
          val n = lenLo + lenHi * 256
          val _ = nextByte r  (* NLEN lo *)
          val _ = nextByte r  (* NLEN hi *)
          fun copy k =
            if k >= n then () else (putByte (nextByte r); copy (k + 1))
        in
          copy 0
        end

      (* dynamic block: read the code-length code lengths, decode lit+dist
         lengths, then build the two decoders (RFC 1951 3.2.7). *)
      val clOrder = Vector.fromList [16,17,18,0,8,7,9,6,10,5,11,4,12,3,13,2,14,1,15]
      fun dynamicBlock () =
        let
          val hlit = getBits r 5 + 257
          val hdist = getBits r 5 + 1
          val hclen = getBits r 4 + 4
          val clLens = Array.array (19, 0)
          fun readCl i =
            if i >= hclen then ()
            else (Array.update (clLens, Vector.sub (clOrder, i), getBits r 3);
                  readCl (i + 1))
          val () = readCl 0
          val clDec = buildDecoder (Array.vector clLens)
          (* decode hlit+hdist code lengths using clDec, handling 16/17/18 *)
          val total = hlit + hdist
          val all = Array.array (total, 0)
          fun readLens i =
            if i >= total then ()
            else
              let val sym = clDec r in
                if sym <= 15 then (Array.update (all, i, sym); readLens (i + 1))
                else if sym = 16 then
                  let
                    val rep = getBits r 2 + 3
                    val prev = if i = 0 then raise Bad else Array.sub (all, i - 1)
                    fun fill (k, j) =
                      if k >= rep orelse j >= total then j
                      else (Array.update (all, j, prev); fill (k + 1, j + 1))
                  in
                    readLens (fill (0, i))
                  end
                else if sym = 17 then
                  let
                    val rep = getBits r 3 + 3
                    fun fill (k, j) =
                      if k >= rep orelse j >= total then j
                      else (Array.update (all, j, 0); fill (k + 1, j + 1))
                  in
                    readLens (fill (0, i))
                  end
                else (* sym = 18 *)
                  let
                    val rep = getBits r 7 + 11
                    fun fill (k, j) =
                      if k >= rep orelse j >= total then j
                      else (Array.update (all, j, 0); fill (k + 1, j + 1))
                  in
                    readLens (fill (0, i))
                  end
              end
          val () = readLens 0
          val litLens = Vector.tabulate (hlit, fn i => Array.sub (all, i))
          val distLens = Vector.tabulate (hdist, fn i => Array.sub (all, hlit + i))
        in
          inflateBlock (buildDecoder litLens, buildDecoder distLens)
        end

      fun blocks () =
        let
          val bfinal = getBit r
          val btype = getBits r 2
        in
          (case btype of
               0 => storedBlock ()
             | 1 => inflateBlock (buildDecoder (fixedLitLengths ()),
                                  buildDecoder (fixedDistLengths ()))
             | 2 => dynamicBlock ()
             | _ => raise Bad);
          if bfinal = 1 then () else blocks ()
        end
    in
      (blocks ();
       SOME (CharArraySlice.vector
               (CharArraySlice.slice (!buf, 0, SOME (!len)))))
      handle Bad => NONE
           | Subscript => NONE
           | Overflow => NONE
    end
end
