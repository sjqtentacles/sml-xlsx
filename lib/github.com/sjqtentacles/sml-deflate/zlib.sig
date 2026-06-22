(* zlib.sig

   RFC 1950 (zlib) and RFC 1952 (gzip) container wrappers around raw DEFLATE,
   in both directions. The decoders strip the container header, inflate the
   body, and verify the trailer checksum/length. The encoders wrap a
   `Deflate.deflate` body with the appropriate header and Adler-32 / CRC-32 +
   ISIZE trailer; `inflateZlib (deflateZlib x) = SOME x` and
   `inflateGzip (gzip x) = SOME x` for all inputs.

   `Adler32` is exposed because zlib's trailer uses it (sml-codec provides
   CRC-32 for gzip, but not Adler-32). *)

signature ZLIB =
sig
  (* zlib (RFC 1950): 2-byte header, DEFLATE body, 4-byte Adler-32 trailer. *)
  val inflateZlib : string -> string option
  (* gzip (RFC 1952): 10+ byte header, DEFLATE body, CRC-32 + ISIZE trailer. *)
  val inflateGzip : string -> string option

  (* Wrap a DEFLATE-compressed body in a zlib (RFC 1950) container. *)
  val deflateZlib : {level : int} -> string -> string
  (* Wrap a DEFLATE-compressed body in a gzip (RFC 1952) container. *)
  val gzip : {level : int} -> string -> string

  (* Adler-32 checksum (RFC 1950 9). Exposed for reuse/testing. *)
  val adler32 : string -> Word32.word
end
