(* inflate.sig

   RFC 1951 DEFLATE decompression (inflate). Pure: takes the raw deflate
   stream as a byte string and returns the decompressed bytes, or NONE on a
   malformed stream. Supports stored, fixed-Huffman, and dynamic-Huffman
   blocks with LZ77 back-references. *)

signature INFLATE =
sig
  (* Inflate a raw DEFLATE (RFC 1951) byte string. NONE on malformed input. *)
  val inflate : string -> string option
end
