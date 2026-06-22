(* unicode.sig

   Pure-SML Unicode utilities: codecs (UTF-8 / UTF-16), canonical
   normalization (NFC / NFD), simple case folding, extended grapheme-cluster
   segmentation, and East-Asian display width.

   Codepoints are represented as plain `int`s (Unicode scalar values, i.e.
   0x0..0x10FFFF excluding the surrogate range 0xD800..0xDFFF). Byte strings
   are ordinary SML `string`s (a sequence of 8-bit `char`s); the codecs map
   between such byte strings and codepoint lists.

   The normalization / case / width operations are backed by a *curated
   subset* of the Unicode Character Database (see `data.sml` and the README's
   "Scope of Unicode data shipped" section). They are correct on the covered
   ranges and degrade gracefully (identity / width 1) outside them. *)

signature UNICODE =
sig
  (* Raised by the decoders on malformed input (e.g. a lone UTF-8
     continuation byte, a truncated multi-byte sequence, an odd-length
     UTF-16 byte string, or an unpaired surrogate). *)
  exception Malformed of string

  (* ---- UTF-8 ---- *)

  (* Decode a UTF-8 byte string to its list of codepoints. Raises
     `Malformed` on invalid/overlong/truncated sequences. *)
  val decodeUtf8 : string -> int list

  (* Encode a list of codepoints as a UTF-8 byte string. Raises `Malformed`
     on out-of-range codepoints or surrogate scalar values. *)
  val encodeUtf8 : int list -> string

  (* ---- UTF-16 ---- *)

  (* Byte order for the UTF-16 codecs. *)
  datatype endian = BE | LE

  (* Decode a UTF-16 byte string (2 bytes per code unit, surrogate pairs
     combined) to codepoints. Raises `Malformed` on odd length or an
     unpaired surrogate. *)
  val decodeUtf16 : endian -> string -> int list

  (* Encode codepoints as a UTF-16 byte string, emitting surrogate pairs for
     supplementary-plane codepoints. *)
  val encodeUtf16 : endian -> int list -> string

  (* ---- Normalization ---- *)

  datatype form = NFC | NFD

  (* Canonical normalization. NFD fully (canonically) decomposes and
     canonically orders combining marks; NFC additionally recomposes. *)
  val normalize : form -> int list -> int list

  (* ---- Case folding ---- *)

  (* Simple (common-case) case folding: maps each codepoint to its folded
     form for case-insensitive comparison within the covered set. *)
  val caseFold : int list -> int list

  (* ---- Segmentation ---- *)

  (* Split a UTF-8 string into extended grapheme clusters (UAX #29, common
     rules), each returned as a UTF-8 substring. *)
  val graphemes : string -> string list

  (* ---- Width ---- *)

  (* Monospace display width of a codepoint: 0 for combining / zero-width,
     2 for East-Asian wide / fullwidth, otherwise 1. *)
  val width : int -> int
end
