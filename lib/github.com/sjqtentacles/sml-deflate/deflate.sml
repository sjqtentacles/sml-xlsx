(* deflate.sml -- RFC 1951 DEFLATE compression.

   A correctness-first encoder mirroring the structure of inflate.sml:

     - a bit writer that emits the input LSB-first (3.1.1), with a helper
       that emits Huffman codes MSB-first (the only big-endian element);
     - LZ77 match finding with a hash table + chains over a 32 KB window,
       producing a stream of literal / (length, distance) tokens (3.2.5);
     - canonical Huffman code construction from code lengths (3.2.2), shared
       by the fixed trees (3.2.6) and the per-block dynamic trees (3.2.7);
     - length-limited (<= 15-bit) Huffman length generation for dynamic
       blocks, with the run-length encoding of the code-length alphabet.

   `deflate` builds the candidate encodings (stored, fixed, dynamic) for the
   token stream and keeps the smallest, so the result is always valid and no
   worse than stored. Everything is pure and deterministic. *)

structure Deflate :> DEFLATE =
struct
  (* ----- shared length/distance tables (RFC 1951 3.2.5) ----- *)
  val lenBase = Vector.fromList [3,4,5,6,7,8,9,10,11,13,15,17,19,23,27,31,35,43,51,59,
                  67,83,99,115,131,163,195,227,258]
  val lenExtra = Vector.fromList [0,0,0,0,0,0,0,0,1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4,5,5,5,5,0]
  val distBase = Vector.fromList [1,2,3,4,5,7,9,13,17,25,33,49,65,97,129,193,257,385,513,769,
                   1025,1537,2049,3073,4097,6145,8193,12289,16385,24577]
  val distExtra = Vector.fromList [0,0,0,0,1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9,10,10,11,11,
                    12,12,13,13]
  (* order in which code-length code lengths are transmitted (3.2.7) *)
  val clOrder = Vector.fromList [16,17,18,0,8,7,9,6,10,5,11,4,12,3,13,2,14,1,15]

  (* powers of two for bit twiddling (index 0..15) *)
  val pow2 = Vector.fromList [1,2,4,8,16,32,64,128,256,512,1024,2048,4096,8192,16384,32768]
  fun p2 i = Vector.sub (pow2, i)

  (* ----- bit writer ----- *)
  (* state: growable byte buffer, current sub-byte accumulator + bit count *)
  type writer =
    { buf : CharArray.array ref, cap : int ref, len : int ref,
      acc : int ref, nbits : int ref }

  fun newWriter () : writer =
    { buf = ref (CharArray.array (1024, #"\000")), cap = ref 1024,
      len = ref 0, acc = ref 0, nbits = ref 0 }

  fun ensure (w : writer) extra =
    if !(#len w) + extra <= !(#cap w) then ()
    else
      let
        val newCap = ref (!(#cap w))
        val () = while !(#len w) + extra > !newCap do newCap := !newCap * 2
        val nb = CharArray.array (!newCap, #"\000")
      in
        CharArray.copy { src = !(#buf w), dst = nb, di = 0 };
        #buf w := nb; #cap w := !newCap
      end

  fun emitByte (w : writer) v =
    (ensure w 1;
     CharArray.update (!(#buf w), !(#len w), Char.chr (v mod 256));
     #len w := !(#len w) + 1)

  (* push a single bit into the accumulator, flushing a full byte LSB-first *)
  fun pushBit (w : writer) b =
    (#acc w := !(#acc w) + (if b <> 0 then p2 (!(#nbits w)) else 0);
     #nbits w := !(#nbits w) + 1;
     if !(#nbits w) = 8
     then (emitByte w (!(#acc w)); #acc w := 0; #nbits w := 0)
     else ())

  (* write n bits of `value`, least-significant bit first (non-Huffman data) *)
  fun writeBits w (value, n) =
    let
      fun loop (i, v) =
        if i >= n then () else (pushBit w (v mod 2); loop (i + 1, v div 2))
    in
      loop (0, value)
    end

  (* write a Huffman code of `len` bits, most-significant bit first (3.1.1) *)
  fun writeCode w (code, len) =
    let
      fun loop i =
        if i < 0 then () else (pushBit w ((code div p2 i) mod 2); loop (i - 1))
    in
      loop (len - 1)
    end

  (* pad the final partial byte with zero bits *)
  fun align (w : writer) =
    if !(#nbits w) > 0
    then (emitByte w (!(#acc w)); #acc w := 0; #nbits w := 0)
    else ()

  fun finish (w : writer) =
    (align w;
     CharArraySlice.vector (CharArraySlice.slice (!(#buf w), 0, SOME (!(#len w)))))

  (* ----- canonical Huffman codes from code lengths (3.2.2) ----- *)
  (* Returns a per-symbol code array (0 where the length is 0/unused). *)
  fun buildCanon (lengths : int vector) =
    let
      val n = Vector.length lengths
      val maxBits = Vector.foldl Int.max 0 lengths
      val blCount = Array.array (maxBits + 1, 0)
      val () = Vector.app
                 (fn len => if len > 0
                            then Array.update (blCount, len, Array.sub (blCount, len) + 1)
                            else ()) lengths
      val nextCode = Array.array (maxBits + 1, 0)
      val () =
        let
          fun loop (bits, code) =
            if bits > maxBits then ()
            else
              let val code' = (code + Array.sub (blCount, bits - 1)) * 2 in
                Array.update (nextCode, bits, code');
                loop (bits + 1, code')
              end
        in
          if maxBits >= 1 then loop (1, 0) else ()
        end
      val codes = Array.array (n, 0)
      val () =
        let
          fun loop sym =
            if sym >= n then ()
            else
              let val len = Vector.sub (lengths, sym) in
                (if len <> 0 then
                   (Array.update (codes, sym, Array.sub (nextCode, len));
                    Array.update (nextCode, len, Array.sub (nextCode, len) + 1))
                 else ());
                loop (sym + 1)
              end
        in
          loop 0
        end
    in
      codes
    end

  (* map a match length (3..258) to (symbol, #extra bits, extra value) *)
  fun lenInfo len =
    let
      fun loop i =
        if i + 1 < 29 andalso len >= Vector.sub (lenBase, i + 1) then loop (i + 1) else i
      val i = loop 0
    in
      (257 + i, Vector.sub (lenExtra, i), len - Vector.sub (lenBase, i))
    end

  (* map a distance (1..32768) to (symbol, #extra bits, extra value) *)
  fun distInfo dist =
    let
      fun loop i =
        if i + 1 < 30 andalso dist >= Vector.sub (distBase, i + 1) then loop (i + 1) else i
      val i = loop 0
    in
      (i, Vector.sub (distExtra, i), dist - Vector.sub (distBase, i))
    end

  (* fixed Huffman code lengths (3.2.6) *)
  val fixedLitLengths =
    Vector.tabulate (288, fn i =>
      if i <= 143 then 8 else if i <= 255 then 9 else if i <= 279 then 7 else 8)
  val fixedDistLengths = Vector.tabulate (30, fn _ => 5)

  (* ----- LZ77 tokens ----- *)
  datatype token = Lit of int | Mat of int * int   (* literal byte | (len, dist) *)

  val hashSize = 32768               (* hash table buckets (3-byte keys) *)
  val minMatch = 3
  val maxMatch = 258
  val maxDist = 32768

  fun chainFor level =
    case level of
        1 => 16 | 2 => 32 | 3 => 64 | 4 => 128 | 5 => 256
      | 6 => 512 | 7 => 1024 | 8 => 2048 | _ => 4096
  fun niceFor level =
    if level <= 2 then 32 else if level <= 4 then 64 else if level <= 6 then 128 else 258

  (* Tokenise `s` with greedy hash-chain match finding. *)
  fun lz77 (s, level) : token vector =
    let
      val n = String.size s
      val maxChain = chainFor level
      val niceLen = niceFor level
      fun b i = Char.ord (String.sub (s, i))

      val head = Array.array (hashSize, ~1)
      val prev = Array.array (Int.max (n, 1), ~1)
      fun hash i =
        (b i * 7919 + b (i + 1) * 263 + b (i + 2)) mod hashSize

      fun insert i =
        if i + 2 >= n then ()
        else
          let val h = hash i in
            Array.update (prev, i, Array.sub (head, h));
            Array.update (head, h, i)
          end

      (* growable token buffer *)
      val tcap = ref 256
      val tarr = ref (Array.array (!tcap, Lit 0))
      val tlen = ref 0
      fun push tok =
        (if !tlen >= !tcap then
           let val nc = !tcap * 2
               val na = Array.array (nc, Lit 0)
           in Array.copy { src = !tarr, dst = na, di = 0 }; tarr := na; tcap := nc end
         else ();
         Array.update (!tarr, !tlen, tok); tlen := !tlen + 1)

      fun findMatch i =
        if i + minMatch > n then (0, 0)
        else
          let
            val h = hash i
            val limit = Int.min (maxMatch, n - i)
            fun chain (cand, depth, bestLen, bestDist) =
              if cand < 0 orelse depth <= 0
                 orelse (i - cand) > maxDist orelse bestLen >= limit
              then (bestLen, bestDist)
              else
                let
                  (* quick reject: the byte past the current best must match *)
                  val ok = bestLen = 0
                           orelse b (cand + bestLen) = b (i + bestLen)
                in
                  if not ok
                  then chain (Array.sub (prev, cand), depth - 1, bestLen, bestDist)
                  else
                    let
                      fun ml k =
                        if k >= limit then k
                        else if b (cand + k) = b (i + k) then ml (k + 1) else k
                      val len = ml 0
                      val (bl, bd) =
                        if len > bestLen then (len, i - cand) else (bestLen, bestDist)
                    in
                      if bl >= niceLen then (bl, bd)
                      else chain (Array.sub (prev, cand), depth - 1, bl, bd)
                    end
                end
          in
            chain (Array.sub (head, h), maxChain, 0, 0)
          end

      fun loop i =
        if i >= n then ()
        else if i + minMatch > n then (push (Lit (b i)); insert i; loop (i + 1))
        else
          let val (len, dist) = findMatch i in
            if len >= minMatch then
              let
                fun ins j = if j >= i + len then () else (insert j; ins (j + 1))
              in
                push (Mat (len, dist)); ins i; loop (i + len)
              end
            else (push (Lit (b i)); insert i; loop (i + 1))
          end
    in
      loop 0;
      Vector.tabulate (!tlen, fn i => Array.sub (!tarr, i))
    end

  (* ----- emit a token stream given lit/len and distance code tables ----- *)
  fun emitTokens w (litCode, litLen : int array, distCode, distLen : int array) toks =
    (Vector.app
       (fn Lit byte =>
              writeCode w (Array.sub (litCode, byte), Array.sub (litLen, byte))
         | Mat (len, dist) =>
              let
                val (lsym, leb, lev) = lenInfo len
                val (dsym, deb, dev) = distInfo dist
              in
                writeCode w (Array.sub (litCode, lsym), Array.sub (litLen, lsym));
                if leb > 0 then writeBits w (lev, leb) else ();
                writeCode w (Array.sub (distCode, dsym), Array.sub (distLen, dsym));
                if deb > 0 then writeBits w (dev, deb) else ()
              end)
       toks;
     (* end-of-block *)
     writeCode w (Array.sub (litCode, 256), Array.sub (litLen, 256)))

  (* ----- stored (uncompressed) blocks (3.2.4) ----- *)
  fun buildStored s =
    let
      val w = newWriter ()
      val n = String.size s
      fun block (off, this, isLast) =
        let
          val nlen = Word.toInt (Word.andb (Word.notb (Word.fromInt this), 0wxFFFF))
        in
          writeBits w (if isLast then 1 else 0, 1);
          writeBits w (0, 2);
          align w;
          emitByte w (this mod 256); emitByte w (this div 256);
          emitByte w (nlen mod 256); emitByte w (nlen div 256);
          let fun cp k = if k >= this then ()
                         else (emitByte w (Char.ord (String.sub (s, off + k))); cp (k + 1))
          in cp 0 end
        end
      fun loop off =
        if off >= n then ()
        else
          let
            val this = Int.min (n - off, 65535)
            val isLast = off + this >= n
          in
            block (off, this, isLast); loop (off + this)
          end
    in
      if n = 0 then block (0, 0, true) else loop 0;
      finish w
    end

  (* ----- fixed-Huffman block (3.2.6) ----- *)
  fun buildFixed toks =
    let
      val litCode = buildCanon fixedLitLengths
      val distCode = buildCanon fixedDistLengths
      val litLen = Array.tabulate (288, fn i => Vector.sub (fixedLitLengths, i))
      val distLen = Array.tabulate (30, fn _ => 5)
      val w = newWriter ()
    in
      writeBits w (1, 1);          (* BFINAL = 1 *)
      writeBits w (1, 2);          (* BTYPE  = 01 (fixed) *)
      emitTokens w (litCode, litLen, distCode, distLen) toks;
      finish w
    end

  (* ----- length-limited Huffman length generation (for dynamic blocks) -----
     Build an optimal Huffman tree from symbol frequencies, then cap code
     lengths at `maxBits` using the classic bit-count redistribution, and
     finally assign lengths canonically (shortest to most-frequent). *)
  datatype htree = Leaf of int | Node of htree * htree

  fun genLengths (freq : int array, alpha, maxBits) =
    let
      val lengths = Array.array (alpha, 0)
      val used = ref []
      val () =
        let fun loop i = if i < 0 then ()
                         else (if Array.sub (freq, i) > 0 then used := i :: !used else ();
                               loop (i - 1))
        in loop (alpha - 1) end
      val usedList = !used                 (* ascending symbols *)
      val numUsed = List.length usedList
    in
      if numUsed = 0 then lengths
      else if numUsed = 1 then
        (Array.update (lengths, List.hd usedList, 1); lengths)
      else
        let
          (* repeatedly merge the two lowest-weight nodes *)
          fun extractMin nodes =
            let
              fun go (best, acc, rest) =
                case rest of
                    [] => (best, acc)
                  | x :: xs =>
                      if #1 x < #1 best then go (x, best :: acc, xs)
                      else go (best, x :: acc, xs)
            in
              case nodes of
                  [] => raise Empty
                | x :: xs => go (x, [], xs)
            end
          fun build nodes =
            case nodes of
                [single] => #2 single
              | _ =>
                  let
                    val (m1, r1) = extractMin nodes
                    val (m2, r2) = extractMin r1
                    val merged = (#1 m1 + #1 m2, Node (#2 m1, #2 m2))
                  in
                    build (merged :: r2)
                  end
          val nodes0 =
            List.map (fn sym => (Array.sub (freq, sym), Leaf sym)) usedList
          val tree = build nodes0
          (* depth of each symbol *)
          val depth = Array.array (alpha, 0)
          fun walk (t, d) =
            case t of
                Leaf sym => Array.update (depth, sym, d)
              | Node (a, b) => (walk (a, d + 1); walk (b, d + 1))
          val () = walk (tree, 0)
          val maxDepth =
            List.foldl (fn (sym, m) => Int.max (m, Array.sub (depth, sym))) 0 usedList
          val maxDepth = Int.max (maxDepth, 1)
          (* index up to max(maxDepth, maxBits): clamp reads high depths, the
             final assignment reads every length up to maxBits *)
          val bl = Array.array (Int.max (maxDepth, maxBits) + 1, 0)
          val () = List.app
                     (fn sym => let val d = Array.sub (depth, sym)
                                in Array.update (bl, d, Array.sub (bl, d) + 1) end)
                     usedList
          (* clamp anything beyond maxBits and rebalance to a valid code *)
          val () =
            if maxDepth > maxBits then
              let
                val overflow = ref 0
                val () =
                  let fun loop bits =
                        if bits > maxDepth then ()
                        else (overflow := !overflow + Array.sub (bl, bits);
                              Array.update (bl, maxBits, Array.sub (bl, maxBits)
                                                          + Array.sub (bl, bits));
                              Array.update (bl, bits, 0);
                              loop (bits + 1))
                  in loop (maxBits + 1) end
                fun rebalance () =
                  if !overflow <= 0 then ()
                  else
                    let
                      fun findBits bits =
                        if Array.sub (bl, bits) = 0 then findBits (bits - 1) else bits
                      val bits = findBits (maxBits - 1)
                    in
                      Array.update (bl, bits, Array.sub (bl, bits) - 1);
                      Array.update (bl, bits + 1, Array.sub (bl, bits + 1) + 2);
                      Array.update (bl, maxBits, Array.sub (bl, maxBits) - 1);
                      overflow := !overflow - 2;
                      rebalance ()
                    end
              in
                rebalance ()
              end
            else ()
          (* sort used symbols by frequency desc, then symbol asc *)
          val sorted = Array.fromList usedList
          val () =
            let
              val m = Array.length sorted
              fun key a = (Array.sub (freq, a), a)
              (* greater priority = larger freq, or equal freq and smaller sym *)
              fun gt (a, b) =
                let val (fa, sa) = key a and (fb, sb) = key b in
                  fa > fb orelse (fa = fb andalso sa < sb)
                end
              fun ins i =
                if i >= m then ()
                else
                  let
                    val v = Array.sub (sorted, i)
                    fun shift j =
                      if j >= 0 andalso gt (v, Array.sub (sorted, j))
                      then (Array.update (sorted, j + 1, Array.sub (sorted, j)); shift (j - 1))
                      else Array.update (sorted, j + 1, v)
                  in
                    shift (i - 1); ins (i + 1)
                  end
            in
              ins 1
            end
          (* assign: shortest lengths to the most-frequent symbols *)
          val () =
            let
              val idx = ref 0
              fun assignLen bits =
                if bits > maxBits then ()
                else
                  let
                    val cnt = Array.sub (bl, bits)
                    fun take k =
                      if k >= cnt then ()
                      else (Array.update (lengths, Array.sub (sorted, !idx), bits);
                            idx := !idx + 1; take (k + 1))
                  in
                    take 0; assignLen (bits + 1)
                  end
            in
              assignLen 1
            end
        in
          lengths
        end
    end

  (* A code is usable if it is a complete prefix code, or the single-symbol
     (1-bit) special case that conforming inflate accepts. *)
  fun validPrefix (lens : int array, maxBits) =
    let
      val n = Array.length lens
      val used = ref 0
      val kraft = ref 0
      val () =
        let fun loop i =
              if i >= n then ()
              else (let val l = Array.sub (lens, i) in
                      if l > 0 then (used := !used + 1; kraft := !kraft + p2 (maxBits - l))
                      else ()
                    end; loop (i + 1))
        in loop 0 end
    in
      !used <= 1 orelse !kraft = p2 maxBits
    end

  fun lastNonzero (a : int array) =
    let
      fun loop i =
        if i < 0 then ~1 else if Array.sub (a, i) <> 0 then i else loop (i - 1)
    in
      loop (Array.length a - 1)
    end

  (* run-length encode a code-length sequence into (sym, #extra, extra) items
     using symbols 0-15 (literal), 16 (copy prev 3-6), 17 (zeros 3-10),
     18 (zeros 11-138). *)
  fun buildRLE (combined : int array, seqLen) =
    let
      val acc = ref []
      fun add x = acc := x :: !acc
      fun loop i =
        if i >= seqLen then ()
        else
          let
            val v = Array.sub (combined, i)
            fun runEnd j =
              if j < seqLen andalso Array.sub (combined, j) = v then runEnd (j + 1) else j
            val j = runEnd (i + 1)
            val runlen = j - i
          in
            (if v = 0 then
               let
                 fun zr r =
                   if r <= 0 then ()
                   else if r >= 11 then
                     let val t = Int.min (r, 138) in add (18, 7, t - 11); zr (r - t) end
                   else if r >= 3 then add (17, 3, r - 3)   (* consumes 3..10 *)
                   else (add (0, 0, 0); zr (r - 1))
               in zr runlen end
             else
               (add (v, 0, 0);
                let
                  fun rr r =
                    if r <= 0 then ()
                    else if r >= 3 then
                      let val t = Int.min (r, 6) in add (16, 2, t - 3); rr (r - t) end
                    else (add (v, 0, 0); rr (r - 1))
                in rr (runlen - 1) end));
            loop j
          end
    in
      loop 0; List.rev (!acc)
    end

  (* ----- dynamic-Huffman block (3.2.7); NONE if codes are unusable ----- *)
  fun buildDynamic toks =
    let
      val litFreq = Array.array (286, 0)
      val distFreq = Array.array (30, 0)
      fun bump (a, i) = Array.update (a, i, Array.sub (a, i) + 1)
      val () = Vector.app
                 (fn Lit byte => bump (litFreq, byte)
                   | Mat (len, dist) =>
                       let val (ls, _, _) = lenInfo len
                           val (ds, _, _) = distInfo dist
                       in bump (litFreq, ls); bump (distFreq, ds) end)
                 toks
      val () = bump (litFreq, 256)            (* end-of-block always present *)
      val litLens = genLengths (litFreq, 286, 15)
      val anyDist = lastNonzero distFreq >= 0
      val distLens =
        if anyDist then genLengths (distFreq, 30, 15)
        else let val a = Array.array (30, 0) in Array.update (a, 0, 1); a end
    in
      if not (validPrefix (litLens, 15))
         orelse (anyDist andalso not (validPrefix (distLens, 15)))
      then NONE
      else
        let
          val hlit = Int.max (257, lastNonzero litLens + 1)
          val hdist = Int.max (1, lastNonzero distLens + 1)
          val seqLen = hlit + hdist
          val combined =
            Array.tabulate (seqLen, fn i =>
              if i < hlit then Array.sub (litLens, i) else Array.sub (distLens, i - hlit))
          val rle = buildRLE (combined, seqLen)
          val clFreq = Array.array (19, 0)
          val () = List.app (fn (sym, _, _) =>
                     Array.update (clFreq, sym, Array.sub (clFreq, sym) + 1)) rle
          val clLens = genLengths (clFreq, 19, 7)
        in
          if not (validPrefix (clLens, 7)) then NONE
          else
            let
              val clCode = buildCanon (Array.vector clLens)
              val litCode = buildCanon (Array.vector litLens)
              val distCode = buildCanon (Array.vector distLens)
              val hclen =
                let
                  fun loop k =
                    if k < 0 then 4
                    else if Array.sub (clLens, Vector.sub (clOrder, k)) <> 0
                    then Int.max (k + 1, 4)
                    else loop (k - 1)
                in loop 18 end
              val w = newWriter ()
            in
              writeBits w (1, 1);              (* BFINAL = 1 *)
              writeBits w (2, 2);              (* BTYPE  = 10 (dynamic) *)
              writeBits w (hlit - 257, 5);
              writeBits w (hdist - 1, 5);
              writeBits w (hclen - 4, 4);
              let fun loop k =
                    if k >= hclen then ()
                    else (writeBits w (Array.sub (clLens, Vector.sub (clOrder, k)), 3);
                          loop (k + 1))
              in loop 0 end;
              List.app
                (fn (sym, extraBits, extraVal) =>
                   (writeCode w (Array.sub (clCode, sym), Array.sub (clLens, sym));
                    if extraBits > 0 then writeBits w (extraVal, extraBits) else ()))
                rle;
              emitTokens w (litCode, litLens, distCode, distLens) toks;
              SOME (finish w)
            end
        end
    end

  fun deflate {level} s =
    if level <= 0 then buildStored s
    else
      let
        val toks = lz77 (s, level)
        val stored = buildStored s
        val fixed = buildFixed toks
        fun smaller (a, b) = if String.size a <= String.size b then a else b
        val best = smaller (stored, fixed)
      in
        case buildDynamic toks of
            NONE => best
          | SOME d => smaller (best, d)
      end
end
