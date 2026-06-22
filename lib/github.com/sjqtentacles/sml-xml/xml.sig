(* xml.sig

   A small, pure-Standard-ML XML toolkit: a recursive-descent parser, an
   in-memory DOM, and a well-formed serializer, with XML-namespace support.

   The parser handles elements, attributes (single- or double-quoted), text,
   comments (`<!-- -->`), CDATA sections (`<![CDATA[ ]]>`), the five predefined
   entities (`&lt; &gt; &amp; &quot; &apos;`) and numeric character references
   (`&#nn;` / `&#xhh;`, encoded as UTF-8 via the vendored sml-unicode), empty /
   self-closing tags, and `xmlns` / `xmlns:prefix` namespace declarations with
   prefix resolution. A leading XML declaration (`<?xml ... ?>`), processing
   instructions, and DOCTYPE markup are skipped.

   Namespaces are resolved during parsing: an element's `name` is its *local*
   name (the part after any `prefix:`), and `ns` is the resolved namespace URI
   (the default namespace for an unprefixed name, or the URI bound to its
   prefix). The originating `xmlns` / `xmlns:prefix` declarations are preserved
   verbatim in `attrs`, so serializing and re-parsing yields an equal DOM. *)

signature XML =
sig
  datatype node =
      Element of { name     : string                 (* local element name *)
                 , ns       : string option           (* resolved namespace URI *)
                 , attrs    : (string * string) list  (* raw name, unescaped value *)
                 , children : node list }
    | Text of string        (* character data, already unescaped *)
    | Comment of string     (* the text between <!-- and --> *)
    | CData of string       (* the text between <![CDATA[ and ]]> *)

  (* Raised by `parse` (and the lower-level helpers) on malformed input:
     mismatched / unclosed tags, bad entities, unbound namespace prefixes,
     unterminated comments / CDATA, missing root element, etc. *)
  exception Xml of string

  (* Parse a document and return its root element. Raises `Xml` on malformed
     input or when there is no root element. *)
  val parse : string -> node

  (* Total variant of `parse`: `NONE` instead of raising. *)
  val parseOpt : string -> node option

  (* Serialize a node to a well-formed XML string. Text and attribute values
     are escaped; comments and CDATA are emitted verbatim; namespace prefixes
     are reconstructed from the in-scope `xmlns` declarations. *)
  val render : node -> string

  (* Pre-order collection of every node in the subtree (root included) for
     which the predicate holds. *)
  val findAll : (node -> bool) -> node -> node list

  (* --- DOM helpers --- *)

  (* All descendant elements (and the node itself) whose local name matches. *)
  val byName : string -> node -> node list

  (* The immediate children of an element ([] for non-elements). *)
  val children : node -> node list

  (* Attribute lookup by raw (qualified) attribute name. *)
  val getAttr : node -> string -> string option

  (* The local element name, if the node is an element. *)
  val localName : node -> string option

  (* The resolved namespace URI of an element, if any. *)
  val nsOf : node -> string option

  (* Concatenated character data (Text + CData) of the whole subtree. *)
  val textContent : node -> string

  (* --- Escaping primitives --- *)

  (* Escape `&`, `<`, `>` for a text context. *)
  val escapeText : string -> string

  (* Escape `&`, `<`, `>`, `"` for a double-quoted attribute value. *)
  val escapeAttr : string -> string
end
