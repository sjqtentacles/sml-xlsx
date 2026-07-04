# sml-xlsx

[![CI](https://github.com/sjqtentacles/sml-xlsx/actions/workflows/ci.yml/badge.svg)](https://github.com/sjqtentacles/sml-xlsx/actions/workflows/ci.yml)

Pure Standard ML reader/writer for **`.xlsx` spreadsheets** (Office Open XML) —
an `.xlsx` is a ZIP container of XML parts, so this library is a thin, pure
layer over [`sml-zip`](https://github.com/sjqtentacles/sml-zip) (the PK ZIP
container) and [`sml-xml`](https://github.com/sjqtentacles/sml-xml) (the XML
parser/serializer). No FFI, no C, no external tools: it runs identically under
[MLton](http://mlton.org/) and [Poly/ML](https://www.polyml.org/), and writes
**byte-identical** output on both.

```sml
(* build a one-sheet workbook and serialize it to .xlsx bytes *)
val wb =
  Xlsx.workbook
    [ Xlsx.sheet
        ("Sheet1",
         [ ("A1", Xlsx.Str "Name"),  ("B1", Xlsx.Str "Score"),
           ("A2", Xlsx.Str "alice"), ("B2", Xlsx.Num 1.5),
           ("B3", Xlsx.Formula ("B2*2", Xlsx.Num 3.0)) ]) ]

val bytes = Xlsx.toBytes wb            (* Word8Vector.vector, a valid .xlsx *)

(* read it back into the same model *)
val wb'  = Xlsx.fromBytes bytes
val sh   = hd (#sheets wb')
val v    = Xlsx.getCell sh "B2"        (* SOME (Xlsx.Num 1.5) *)
```

The bytes are a genuine `.xlsx`: `unzip -t` passes and tools like Excel /
LibreOffice / `openpyxl` open them.

## Install

Using [smlpkg](https://github.com/diku-dk/smlpkg):

```sh
smlpkg add github.com/sjqtentacles/sml-xlsx
smlpkg sync
```

Then add the library to your MLB file (it pulls in the vendored `sml-zip` and
`sml-xml` automatically):

```
$(SML_LIB)/basis/basis.mlb
lib/github.com/sjqtentacles/sml-xlsx/src/xlsx.mlb
```

## Features

- **Write** a minimal but valid package: `[Content_Types].xml`, `_rels/.rels`,
  `xl/workbook.xml`, `xl/_rels/workbook.xml.rels`, one
  `xl/worksheets/sheetN.xml` per sheet, and `xl/sharedStrings.xml`. The XML is
  produced with the vendored `Xml.render`; the parts are packaged with the
  vendored `Zip`.
- **Read** an `.xlsx`: unzip with `Zip`, parse parts with `Xml.parse`, and
  recover sheet names, cell references, and cell values. Untrusted shared-string
  indices are bounds-checked, so a corrupt or hostile file degrades gracefully
  instead of crashing the reader (and does so identically on both compilers).
- **Cell value variants**: `Num` (numbers), `Str` (pooled in the shared-string
  table), `Bool`, and `Formula` (formula text plus a cached `Num` / `Str` /
  `Bool` result — there is no evaluation engine).
- **Deterministic & portable**: ZIP mod-times are zeroed and numbers are
  formatted with a fixed, compiler-independent routine, so the same workbook
  always yields byte-identical output under MLton and Poly/ML.
- Vendors `sml-zip` (which itself vendors `sml-deflate` + `sml-codec`) and
  `sml-xml` (which vendors `sml-unicode`), byte-for-byte.

## API

```sml
datatype value =
    Num of real
  | Str of string
  | Bool of bool
  | Formula of string * value          (* formula text, cached value *)

type cell     = string * value         (* (A1-style ref, value) *)
type sheet    = { name : string, cells : cell list }
type workbook = { sheets : sheet list }

exception Xlsx of string

val cell     : string * value -> cell
val sheet    : string * cell list -> sheet
val workbook : sheet list -> workbook

val parts     : workbook -> (string * string) list   (* (partName, xml) *)
val toBytes   : workbook -> Word8Vector.vector
val fromBytes : Word8Vector.vector -> workbook

val sheetNames : workbook -> string list
val cellRefs   : sheet -> string list
val getCell    : sheet -> string -> value option
```

`fromBytes` raises `Xlsx` (or `Zip` / `Xml` from the vendored libraries) on
malformed input. Cells are written grouped row-major; a write→read round-trip
preserves every sheet name and `(ref → value)` mapping (cell ordering within a
sheet is normalized to row-major).

### Supported vs. deferred

Supported: multiple worksheets, the shared-string table (with XML escaping and
`xml:space="preserve"` for whitespace-significant strings), numeric / string /
boolean cells, and formula cells (`<f>` + cached `<v>`, read and written).
Deferred (not implemented): styles/number formats, merged cells, dates as
formatted numbers, defined names, charts, and formula *evaluation*.

## Build & test

```sh
make test        # MLton: build + run the suite
make test-poly   # Poly/ML: run the suite
make all-tests   # both compilers
make example     # write bin/demo.xlsx and read it back
```

Both compilers report `39 passed, 0 failed`. The suite covers **golden XML**
(the generated `xl/worksheets/sheet1.xml` and `xl/sharedStrings.xml` are
asserted byte-for-byte against pinned references), write→read **round-trips**
over every value variant (numbers, escaped strings, shared-string pooling,
booleans, and number/string/bool formulas across multiple sheets), **write
determinism** (byte-identical re-encode), **container** checks (the output
is a real ZIP whose members are the expected parts, and malformed input is
rejected), and **untrusted-input robustness** (a shared-string reference whose
pool index is huge — beyond a 32-bit `int` — out of the table's bounds, or
non-numeric degrades to the empty string instead of crashing the reader, so
reading stays safe and byte-identical across MLton and Poly/ML).

## Example

`make example` builds a small workbook, writes it to `bin/demo.xlsx`, then
reads the archive back and confirms every cell round-trips:

```
Wrote bin/demo.xlsx:

  cell  value
  A1    str "Name"
  B1    str "Score"
  A2    str "alice"
  B2    num 1.5000
  A3    str "bob"
  B3    num 2.0000
  B4    formula =SUM(B2:B3)

sheets: Sheet1
archive size: 1933 bytes; round-trips byte-exact: yes
```

The resulting `bin/demo.xlsx` opens in any standard spreadsheet tool
(`unzip -t` passes; `openpyxl` reads `Name`/`Score`, `alice`/`1.5`, `bob`/`2`,
and the `=SUM(B2:B3)` formula).

## Layout

Layout B (vendored dependencies): own sources in `src/`; dependency trees under
`lib/github.com/sjqtentacles/` — `sml-zip` (vendoring `sml-deflate` +
`sml-codec`) and `sml-xml` (vendoring `sml-unicode`), loaded in dependency
order. The vendored trees are byte-identical to their upstreams (`diff -rq`).

## License

MIT — see [LICENSE](LICENSE).
