(* xlsx.sig

   Pure Standard ML reader/writer for the Office Open XML spreadsheet format
   (`.xlsx`). An `.xlsx` file is a ZIP container (see the vendored `sml-zip`)
   holding a handful of XML parts (see the vendored `sml-xml`):

     [Content_Types].xml          part content-type registry
     _rels/.rels                  package -> workbook relationship
     xl/workbook.xml              the list of sheets
     xl/_rels/workbook.xml.rels   workbook -> worksheet / sharedStrings rels
     xl/worksheets/sheetN.xml     one per sheet, the cell grid
     xl/sharedStrings.xml         the shared-string table

   This library builds a *minimal but valid* workbook: static strings are
   pooled in the shared-string table, numbers / booleans are stored inline,
   and formula cells carry their formula text plus a cached value (there is no
   evaluation engine). Reading reverses the process.

   Writing is deterministic: ZIP mod-times are zeroed (by `sml-zip`) and every
   number is formatted with a fixed, compiler-independent routine, so the same
   workbook always serializes to byte-identical output, under both MLton and
   Poly/ML. *)

signature XLSX =
sig
  (* A cell's value. `Formula (text, cached)` stores the formula source (e.g.
     "A2+B2") together with its last cached result; the cached value is itself
     a `Num` / `Str` / `Bool` (formulas do not nest). *)
  datatype value =
      Num of real
    | Str of string
    | Bool of bool
    | Formula of string * value

  (* A cell is an A1-style reference (e.g. "B2") paired with its value. *)
  type cell = string * value

  (* A worksheet: a name and its (non-empty) cells, in writing order. *)
  type sheet = { name : string, cells : cell list }

  (* A workbook: an ordered, non-empty list of sheets. *)
  type workbook = { sheets : sheet list }

  (* Raised on malformed input or unsupported content while reading, and on a
     structurally invalid workbook while writing. *)
  exception Xlsx of string

  (* --- constructors --- *)

  val cell     : string * value -> cell
  val sheet    : string * cell list -> sheet
  val workbook : sheet list -> workbook

  (* --- parts (exposed for golden testing) --- *)

  (* The XML parts that make up the package, as `(partName, xml)` pairs in a
     fixed order. Each `xml` string is exactly what is stored in the ZIP. *)
  val parts : workbook -> (string * string) list

  (* --- serialize / parse --- *)

  (* Serialize a workbook to the bytes of an `.xlsx` file. *)
  val toBytes : workbook -> Word8Vector.vector

  (* Parse the bytes of an `.xlsx` file back into the workbook model. Raises
     `Xlsx` (or `Zip` / `Xml` from the vendored libraries) on malformed input.
     Untrusted shared-string references are bounds-checked: a cell's pool index
     that is non-numeric, negative, larger than a 32-bit `int`, or out of the
     shared-string table's bounds degrades to the empty string rather than
     raising `Overflow` / `Subscript`, so a hostile or corrupt file cannot crash
     the reader and the result is identical under MLton and Poly/ML. *)
  val fromBytes : Word8Vector.vector -> workbook

  (* --- accessors --- *)

  val sheetNames : workbook -> string list
  val cellRefs   : sheet -> string list
  val getCell    : sheet -> string -> value option
end
