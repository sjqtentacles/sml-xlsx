(* zlib.sml -- RFC 1950 (zlib) and RFC 1952 (gzip) container decoding. *)

structure Zlib :> ZLIB =
struct
  (* ----- Adler-32 (RFC 1950 9) ----- *)
  fun adler32 s =
    let
      val modAdler = 65521
      fun loop (i, a, b) =
        if i >= String.size s then (a, b)
        else
          let
            val a' = (a + Char.ord (String.sub (s, i))) mod modAdler
            val b' = (b + a') mod modAdler
          in
            loop (i + 1, a', b')
          end
      val (a, b) = loop (0, 1, 0)
    in
      Word32.orb (Word32.<< (Word32.fromInt b, 0w16), Word32.fromInt a)
    end

  fun byte s i = Char.ord (String.sub (s, i))

  (* serialise a Word32 as 4 bytes, little- or big-endian *)
  fun w32byte (w, shift) =
    Char.chr (Word32.toInt (Word32.andb (Word32.>> (w, shift), 0wxFF)))
  fun le32 w = String.implode [w32byte (w, 0w0),  w32byte (w, 0w8),
                               w32byte (w, 0w16), w32byte (w, 0w24)]
  fun be32 w = String.implode [w32byte (w, 0w24), w32byte (w, 0w16),
                               w32byte (w, 0w8),  w32byte (w, 0w0)]

  (* ----- zlib encode (RFC 1950) ----- *)
  fun deflateZlib {level} s =
    let
      (* CM = 8 (deflate), CINFO = 7 (32K window) -> CMF = 0x78 *)
      val cmf = 0x78
      val flevel = if level >= 7 then 3 else if level >= 6 then 2
                   else if level >= 2 then 1 else 0
      val flgBase = flevel * 64               (* FLEVEL in the top two bits *)
      (* FCHECK makes (CMF*256 + FLG) a multiple of 31; FDICT = 0 *)
      val fcheck = (31 - (cmf * 256 + flgBase) mod 31) mod 31
      val flg = flgBase + fcheck
      val header = String.implode [Char.chr cmf, Char.chr flg]
      val body = Deflate.deflate {level = level} s
    in
      header ^ body ^ be32 (adler32 s)
    end

  (* ----- gzip encode (RFC 1952) ----- *)
  fun gzip {level} s =
    let
      val xfl = if level >= 7 then 2 else if level <= 1 then 4 else 0
      (* magic, CM=8, FLG=0, MTIME=0 (deterministic), XFL, OS=255 (unknown) *)
      val header = String.implode
        [Char.chr 0x1f, Char.chr 0x8b, Char.chr 0x08, Char.chr 0x00,
         Char.chr 0x00, Char.chr 0x00, Char.chr 0x00, Char.chr 0x00,
         Char.chr xfl, Char.chr 0xff]
      val body = Deflate.deflate {level = level} s
      val isize = Word32.fromInt (String.size s)
    in
      header ^ body ^ le32 (Crc32.crc s) ^ le32 isize
    end

  (* ----- zlib (RFC 1950) ----- *)
  fun inflateZlib s =
    if String.size s < 6 then NONE
    else
      let
        val cmf = byte s 0
        val flg = byte s 1
        val cm = cmf mod 16          (* compression method, must be 8 *)
        val fdict = (flg div 32) mod 2
        (* header check: (cmf*256 + flg) mod 31 = 0 *)
        val headerOk = (cmf * 256 + flg) mod 31 = 0
        val bodyStart = if fdict = 1 then 6 else 2   (* skip DICTID if present *)
      in
        if cm <> 8 orelse not headerOk then NONE
        else
          let
            val bodyLen = String.size s - bodyStart - 4
          in
            if bodyLen < 0 then NONE
            else
              let
                val body = String.substring (s, bodyStart, bodyLen)
              in
                case Inflate.inflate body of
                    NONE => NONE
                  | SOME out =>
                      let
                        val tpos = String.size s - 4
                        (* Adler-32 trailer is big-endian *)
                        val trailer =
                          Word32.orb
                            (Word32.<< (Word32.fromInt (byte s tpos), 0w24),
                             Word32.orb
                               (Word32.<< (Word32.fromInt (byte s (tpos+1)), 0w16),
                                Word32.orb
                                  (Word32.<< (Word32.fromInt (byte s (tpos+2)), 0w8),
                                   Word32.fromInt (byte s (tpos+3)))))
                      in
                        if adler32 out = trailer then SOME out else NONE
                      end
              end
          end
      end

  (* ----- gzip (RFC 1952) ----- *)
  fun inflateGzip s =
    if String.size s < 18 then NONE
    else if byte s 0 <> 0x1f orelse byte s 1 <> 0x8b orelse byte s 2 <> 8
    then NONE
    else
      let
        val flg = byte s 3
        val fextra = (flg div 4) mod 2
        val fname = (flg div 8) mod 2
        val fcomment = (flg div 16) mod 2
        val fhcrc = (flg div 2) mod 2
        (* fixed header is 10 bytes; then optional fields *)
        fun skipExtra pos =
          if fextra = 0 then pos
          else
            let val xlen = byte s pos + byte s (pos + 1) * 256
            in pos + 2 + xlen end
        fun skipZ pos =
          (* skip a zero-terminated string starting at pos *)
          if pos >= String.size s then pos
          else if byte s pos = 0 then pos + 1
          else skipZ (pos + 1)
        val p1 = skipExtra 10
        val p2 = if fname = 1 then skipZ p1 else p1
        val p3 = if fcomment = 1 then skipZ p2 else p2
        val bodyStart = if fhcrc = 1 then p3 + 2 else p3
        val bodyLen = String.size s - bodyStart - 8
      in
        if bodyLen < 0 then NONE
        else
          let
            val body = String.substring (s, bodyStart, bodyLen)
          in
            case Inflate.inflate body of
                NONE => NONE
              | SOME out =>
                  let
                    val tpos = String.size s - 8
                    (* CRC-32 and ISIZE are little-endian *)
                    fun le32 base =
                      Word32.orb
                        (Word32.fromInt (byte s base),
                         Word32.orb
                           (Word32.<< (Word32.fromInt (byte s (base+1)), 0w8),
                            Word32.orb
                              (Word32.<< (Word32.fromInt (byte s (base+2)), 0w16),
                               Word32.<< (Word32.fromInt (byte s (base+3)), 0w24))))
                    val crcExpected = le32 tpos
                    val isize = le32 (tpos + 4)
                    val sizeOk =
                      isize = Word32.fromInt (String.size out)
                  in
                    if Crc32.crc out = crcExpected andalso sizeOk
                    then SOME out else NONE
                  end
          end
      end
end
