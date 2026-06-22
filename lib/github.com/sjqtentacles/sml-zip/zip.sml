(* zip.sml

   PK ZIP archive reader/writer. Little-endian throughout (APPNOTE 4.4).
   Depends on the vendored Crc32 (sml-codec), Deflate and Inflate
   (sml-deflate). *)

structure Zip :> ZIP =
struct
  datatype method = Stored | Deflated

  type entry = { name : string, contents : Word8Vector.vector, method : method }

  datatype archive = Archive of entry list

  exception Zip of string

  (* PK signatures. *)
  val sigLocal   = 0x04034b50  (* local file header *)
  val sigCentral = 0x02014b50  (* central directory file header *)
  val sigEOCD    = 0x06054b50  (* end of central directory *)

  (* --- byte <-> string helpers (byte-preserving) --- *)
  fun vecToStr v = Byte.bytesToString v
  fun strToVec s = Byte.stringToBytes s

  (* ===================== writing ===================== *)

  (* Little-endian byte lists (as ints 0..255). *)
  fun le16 n = [n mod 256, (n div 256) mod 256]
  fun le32 n =
    [ n mod 256, (n div 256) mod 256,
      (n div 65536) mod 256, (n div 16777216) mod 256 ]
  fun le32w (w : Word32.word) =
    let fun b k = Word32.toInt (Word32.andb (Word32.>> (w, k), 0wxFF))
    in [b 0w0, b 0w8, b 0w16, b 0w24] end

  fun bytes ns = Word8Vector.fromList (List.map Word8.fromInt ns)

  (* Precomputed per-entry info needed for both the local headers and the
     central directory. *)
  type rec_ =
    { name : string,
      crc : Word32.word,
      usize : int,
      csize : int,
      methodCode : int,
      comp : Word8Vector.vector,
      offset : int }

  fun compress (level, method, contents) =
    case method of
        Stored => (0, contents)
      | Deflated =>
          (8, strToVec (Deflate.deflate {level = level} (vecToStr contents)))

  fun writeEntries (es, level) =
    let
      (* Mutable output: chunks accumulated in reverse, with a running byte
         position so we can record local-header offsets. *)
      val chunks = ref ([] : Word8Vector.vector list)
      val pos = ref 0
      fun emit v = (chunks := v :: !chunks; pos := !pos + Word8Vector.length v)

      (* Local file header (30 bytes) + name + data; returns the record for
         the central directory. *)
      fun emitLocal { name, contents, method } =
        let
          val csum = Crc32.crc (vecToStr contents)
          val usize = Word8Vector.length contents
          val (methodCode, comp) = compress (level, method, contents)
          val csize = Word8Vector.length comp
          val nameVec = strToVec name
          val nameLen = Word8Vector.length nameVec
          val offset = !pos
          val header =
            bytes (le32 sigLocal      (* signature *)
                 @ le16 20            (* version needed *)
                 @ le16 0             (* general purpose flags *)
                 @ le16 methodCode    (* compression method *)
                 @ le16 0             (* mod time (zeroed: deterministic) *)
                 @ le16 0             (* mod date (zeroed: deterministic) *)
                 @ le32w csum         (* CRC-32 *)
                 @ le32 csize         (* compressed size *)
                 @ le32 usize         (* uncompressed size *)
                 @ le16 nameLen       (* file name length *)
                 @ le16 0)            (* extra field length *)
        in
          emit header; emit nameVec; emit comp;
          { name = name, crc = csum, usize = usize, csize = csize,
            methodCode = methodCode, comp = comp, offset = offset }
        end

      val recs = List.map emitLocal es

      (* Central directory. *)
      val cdStart = !pos
      fun emitCentral (r : rec_) =
        let
          val nameVec = strToVec (#name r)
          val nameLen = Word8Vector.length nameVec
          val header =
            bytes (le32 sigCentral    (* signature *)
                 @ le16 20            (* version made by *)
                 @ le16 20            (* version needed *)
                 @ le16 0             (* general purpose flags *)
                 @ le16 (#methodCode r)
                 @ le16 0             (* mod time *)
                 @ le16 0             (* mod date *)
                 @ le32w (#crc r)
                 @ le32 (#csize r)
                 @ le32 (#usize r)
                 @ le16 nameLen
                 @ le16 0             (* extra length *)
                 @ le16 0             (* comment length *)
                 @ le16 0             (* disk number start *)
                 @ le16 0             (* internal attributes *)
                 @ le32 0             (* external attributes *)
                 @ le32 (#offset r)) (* local header offset *)
        in
          emit header; emit nameVec
        end

      val () = List.app emitCentral recs
      val cdSize = !pos - cdStart
      val total = List.length recs

      (* End of central directory record (22 bytes, empty comment). *)
      val eocd =
        bytes (le32 sigEOCD
             @ le16 0                (* this disk number *)
             @ le16 0                (* disk with central dir start *)
             @ le16 total            (* entries on this disk *)
             @ le16 total            (* total entries *)
             @ le32 cdSize
             @ le32 cdStart          (* central dir offset *)
             @ le16 0)               (* comment length *)
      val () = emit eocd
    in
      Word8Vector.concat (List.rev (!chunks))
    end

  fun write { entries = es, level } = writeEntries (es, level)

  (* ===================== reading ===================== *)

  fun rd8 (v, i) =
    if i < 0 orelse i >= Word8Vector.length v then raise Zip "unexpected end of input"
    else Word8.toInt (Word8Vector.sub (v, i))
  fun rd16 (v, i) = rd8 (v, i) + rd8 (v, i + 1) * 256
  fun rd32 (v, i) =
    rd8 (v, i) + rd8 (v, i + 1) * 256
    + rd8 (v, i + 2) * 65536 + rd8 (v, i + 3) * 16777216
  fun rd32w (v, i) =
    let
      val a = Word32.fromInt (rd8 (v, i))
      val b = Word32.fromInt (rd8 (v, i + 1))
      val c = Word32.fromInt (rd8 (v, i + 2))
      val d = Word32.fromInt (rd8 (v, i + 3))
    in
      Word32.orb (a,
        Word32.orb (Word32.<< (b, 0w8),
          Word32.orb (Word32.<< (c, 0w16), Word32.<< (d, 0w24))))
    end

  (* signature match using Word32 (avoids Int overflow when scanning over
     arbitrary 4-byte windows on 32-bit-int compilers) *)
  fun sigAt (v, i, s) =
    i >= 0 andalso i + 4 <= Word8Vector.length v
    andalso rd32w (v, i) = Word32.fromInt s

  fun slice (v, start, len) =
    if start < 0 orelse len < 0 orelse start + len > Word8Vector.length v
    then raise Zip "record extends past end of input"
    else Word8VectorSlice.vector (Word8VectorSlice.slice (v, start, SOME len))

  fun nameAt (v, start, len) = vecToStr (slice (v, start, len))

  fun inflateData comp =
    case Inflate.inflate (vecToStr comp) of
        SOME s => strToVec s
      | NONE => raise Zip "inflate failed"

  fun read v =
    let
      val n = Word8Vector.length v

      (* Scan backwards for the EOCD signature (comment is assumed empty in
         the common case; this finds the last occurrence). *)
      fun findEOCD i =
        if i < 0 then raise Zip "no end-of-central-directory record"
        else if sigAt (v, i, sigEOCD) then i
        else findEOCD (i - 1)
      val eocd = findEOCD (n - 22)

      val total = rd16 (v, eocd + 10)
      val cdOffset = rd32 (v, eocd + 16)

      fun readOne off =
        let
          val () =
            if sigAt (v, off, sigCentral) then ()
            else raise Zip "bad central directory signature"
          val methodCode = rd16 (v, off + 10)
          val crc = rd32w (v, off + 16)
          val csize = rd32 (v, off + 20)
          val usize = rd32 (v, off + 24)
          val nameLen = rd16 (v, off + 28)
          val extraLen = rd16 (v, off + 30)
          val commentLen = rd16 (v, off + 32)
          val lho = rd32 (v, off + 42)
          val name = nameAt (v, off + 46, nameLen)

          (* Locate the entry data via the local file header. *)
          val () =
            if sigAt (v, lho, sigLocal) then ()
            else raise Zip "bad local file header signature"
          val lhNameLen = rd16 (v, lho + 26)
          val lhExtraLen = rd16 (v, lho + 28)
          val dataStart = lho + 30 + lhNameLen + lhExtraLen
          val comp = slice (v, dataStart, csize)

          val (method, contents) =
            case methodCode of
                0 => (Stored, comp)
              | 8 => (Deflated, inflateData comp)
              | m => raise Zip ("unsupported compression method "
                                ^ Int.toString m)
          val () =
            if Word8Vector.length contents = usize then ()
            else raise Zip "uncompressed size mismatch"
          val () =
            if Crc32.crc (vecToStr contents) = crc then ()
            else raise Zip "CRC-32 mismatch"

          val next = off + 46 + nameLen + extraLen + commentLen
        in
          ({ name = name, contents = contents, method = method }, next)
        end

      fun loop (k, off, acc) =
        if k >= total then List.rev acc
        else let val (e, next) = readOne off in loop (k + 1, next, e :: acc) end
    in
      Archive (loop (0, cdOffset, []))
    end

  (* ===================== accessors / helpers ===================== *)

  fun entries (Archive es) = es
  fun names (Archive es) = List.map #name es
  fun find (Archive es) name =
    List.find (fn (e : entry) => #name e = name) es

  fun stored (name, contents) =
    { name = name, contents = contents, method = Stored }
  fun deflated (name, contents) =
    { name = name, contents = contents, method = Deflated }

  fun archive es = Archive es
  fun toBytes { archive = Archive es, level } = writeEntries (es, level)
end
