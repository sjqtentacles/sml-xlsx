(* deflate.sig

   RFC 1951 DEFLATE compression (deflate). Pure: takes the raw bytes as a
   string and returns a raw DEFLATE stream (no zlib/gzip container). The
   `level` field selects the effort/strategy:

     - level <= 0 : stored (uncompressed) blocks only, per RFC 1951 3.2.4.
     - level 1..9 : LZ77 match finding (hash chains) plus Huffman coding,
                    emitting whichever of stored / fixed-Huffman /
                    dynamic-Huffman blocks is smallest. Higher levels search
                    longer match chains.

   The output is always valid raw DEFLATE that `Inflate.inflate` (and any
   conforming inflate, e.g. zlib's) can decompress. Encoding is deterministic:
   the same input and level always yield the same bytes. *)

signature DEFLATE =
sig
  (* Compress a byte string to a raw DEFLATE (RFC 1951) stream. *)
  val deflate : {level : int} -> string -> string
end
