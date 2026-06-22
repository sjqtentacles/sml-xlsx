(* zip.sig

   Pure Standard ML reader/writer for PK ZIP archives (PKWARE APPNOTE).

   An archive is a flat sequence of file entries, each carrying a name (the
   path within the archive), the uncompressed bytes, and the storage method.
   Two methods are supported: `Stored` (method 0, no compression) and
   `Deflated` (method 8, RFC 1951 DEFLATE via the vendored `sml-deflate`).
   Every entry's CRC-32 (vendored `sml-codec`) is written into the local file
   header and the central directory, and is re-verified when reading.

   This is distinct from `sml-gzip`/`sml-deflate`'s zlib/gzip stream wrappers:
   those are single-member RFC 1950/1952 streams, whereas this builds the PK
   container with local file headers, a central directory, and an
   end-of-central-directory record.

   Writing is deterministic: the same entries and level always produce the
   same bytes (DOS mod-time/-date fields are zeroed). *)

signature ZIP =
sig
  (* Storage method for an entry. *)
  datatype method = Stored | Deflated

  (* A single archive member. `name` is the in-archive path (e.g. "a/b.txt");
     `contents` is the uncompressed payload; `method` selects how it is
     stored when writing. *)
  type entry = { name : string, contents : Word8Vector.vector, method : method }

  (* An opaque, in-memory archive (a sequence of entries). *)
  type archive

  (* Raised on malformed input or an internal inconsistency (bad signature,
     truncated record, failed inflate, CRC-32 mismatch). *)
  exception Zip of string

  (* --- reading --- *)

  (* Parse a ZIP archive from its raw bytes. Decompresses each entry and
     verifies its CRC-32. Raises `Zip` on malformed input. *)
  val read : Word8Vector.vector -> archive

  (* The entries of an archive, in central-directory order. *)
  val entries : archive -> entry list

  (* The entry names, in central-directory order. *)
  val names : archive -> string list

  (* Look up an entry by exact name. *)
  val find : archive -> string -> entry option

  (* --- writing --- *)

  (* Serialize entries to a ZIP archive. `level` is the DEFLATE level (0..9)
     used for `Deflated` entries; `Stored` entries ignore it. *)
  val write : { entries : entry list, level : int } -> Word8Vector.vector

  (* --- convenience --- *)

  (* Build a `Stored` / `Deflated` entry from a name and its bytes. *)
  val stored   : string * Word8Vector.vector -> entry
  val deflated : string * Word8Vector.vector -> entry

  (* Build an archive value directly from a list of entries (for writing). *)
  val archive : entry list -> archive

  (* Serialize an `archive` value (deflating any `Deflated` entries). *)
  val toBytes : { archive : archive, level : int } -> Word8Vector.vector
end
