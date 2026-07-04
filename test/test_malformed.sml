(* test_malformed.sml -- an untrusted .xlsx must not crash the reader.

   A shared-string cell (`t="s"`) carries a *pool index* in its `<v>` element.
   A hostile or corrupt file can put anything there: an integer far larger than
   the machine `int` (which overflows a 32-bit `Int.fromString` on MLton while
   silently succeeding on Poly/ML), a non-numeric string, or an in-range integer
   that points past the end of the shared-string table. The reader must degrade
   to its documented empty-string fallback for a malformed shared-string cell --
   never raise `Overflow`, `Option`, or `Subscript` -- and must stay identical
   across compilers. A well-formed reference must still resolve normally. *)

structure MalformedTests =
struct
  open Support
  structure X = Xlsx

  (* Assemble a minimal, structurally valid .xlsx whose single sheet's cell A1
     is a shared-string reference (`t="s"`) whose `<v>` body is `idxStr`, over a
     shared-string table with exactly one entry ("hello", pool index 0). Only
     `idxStr` varies, so each test isolates the index-handling path. *)
  fun bytesWithSstIndex idxStr =
    let
      val decl = Sample.xmlDecl
      val mainNs = Sample.mainNs
      val odRel =
        "http://schemas.openxmlformats.org/officeDocument/2006/relationships"
      val pkgRel =
        "http://schemas.openxmlformats.org/package/2006/content-types"
      val relNs =
        "http://schemas.openxmlformats.org/package/2006/relationships"

      val contentTypes =
        decl
        ^ "<Types xmlns=\"" ^ pkgRel ^ "\">"
        ^ "<Default Extension=\"rels\" ContentType=\"application/vnd.openxmlformats-package.relationships+xml\"/>"
        ^ "<Default Extension=\"xml\" ContentType=\"application/xml\"/>"
        ^ "<Override PartName=\"/xl/workbook.xml\" ContentType=\"application/vnd.openxmlformats-officedocument.spreadsheetml.sheet.main+xml\"/>"
        ^ "<Override PartName=\"/xl/worksheets/sheet1.xml\" ContentType=\"application/vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml\"/>"
        ^ "<Override PartName=\"/xl/sharedStrings.xml\" ContentType=\"application/vnd.openxmlformats-officedocument.spreadsheetml.sharedStrings+xml\"/>"
        ^ "</Types>"

      val rootRels =
        decl
        ^ "<Relationships xmlns=\"" ^ relNs ^ "\">"
        ^ "<Relationship Id=\"rId1\" Type=\"" ^ odRel ^ "/officeDocument\" Target=\"xl/workbook.xml\"/>"
        ^ "</Relationships>"

      val workbook =
        decl
        ^ "<workbook xmlns=\"" ^ mainNs ^ "\" xmlns:r=\"" ^ odRel ^ "\">"
        ^ "<sheets><sheet name=\"Sheet1\" sheetId=\"1\" r:id=\"rId1\"/></sheets>"
        ^ "</workbook>"

      val workbookRels =
        decl
        ^ "<Relationships xmlns=\"" ^ relNs ^ "\">"
        ^ "<Relationship Id=\"rId1\" Type=\"" ^ odRel ^ "/worksheet\" Target=\"worksheets/sheet1.xml\"/>"
        ^ "<Relationship Id=\"rId2\" Type=\"" ^ odRel ^ "/sharedStrings\" Target=\"sharedStrings.xml\"/>"
        ^ "</Relationships>"

      val sheet1 =
        decl
        ^ "<worksheet xmlns=\"" ^ mainNs ^ "\"><sheetData>"
        ^ "<row r=\"1\"><c r=\"A1\" t=\"s\"><v>" ^ idxStr ^ "</v></c></row>"
        ^ "</sheetData></worksheet>"

      val sharedStrings =
        decl
        ^ "<sst xmlns=\"" ^ mainNs ^ "\" count=\"1\" uniqueCount=\"1\">"
        ^ "<si><t>hello</t></si>"
        ^ "</sst>"

      val entries =
        List.map (fn (n, s) => Zip.deflated (n, vec s))
          [ ("[Content_Types].xml", contentTypes),
            ("_rels/.rels", rootRels),
            ("xl/workbook.xml", workbook),
            ("xl/_rels/workbook.xml.rels", workbookRels),
            ("xl/worksheets/sheet1.xml", sheet1),
            ("xl/sharedStrings.xml", sharedStrings) ]
    in
      Zip.write { entries = entries, level = 6 }
    end

  (* Read A1's value from the single sheet of a freshly parsed workbook. *)
  fun a1Of idxStr =
    let
      val wb = X.fromBytes (bytesWithSstIndex idxStr)
      val sh = hd (#sheets wb)
    in
      X.getCell sh "A1"
    end

  (* Assert that forcing the thunk does NOT raise (the complement of
     Harness.checkRaises), using only the existing harness primitives. *)
  fun checkNoRaise name thunk =
    Harness.check name ((ignore (thunk ()); true) handle _ => false)

  fun run () =
    let
      val _ = Harness.section "untrusted shared-string index does not crash the reader"

      (* control: a valid index resolves to its pooled string. *)
      val () = checkValue "valid SST index 0 resolves to \"hello\""
        (X.Str "hello", valOf (a1Of "0"))

      (* (a) an index far larger than 2^31 must not raise Overflow (MLton) nor
         silently succeed (Poly/ML): it degrades to the empty-string fallback. *)
      val () = checkNoRaise "huge (>2^31) SST index does not raise"
        (fn () => a1Of "9999999999")
      val () = checkValue "huge SST index -> empty-string fallback"
        (X.Str "", valOf (a1Of "9999999999"))

      (* (b) an in-range integer that is out of the table's bounds must not raise
         Subscript: it degrades to the same empty-string fallback. *)
      val () = checkNoRaise "out-of-bounds SST index does not raise"
        (fn () => a1Of "5")
      val () = checkValue "out-of-bounds SST index -> empty-string fallback"
        (X.Str "", valOf (a1Of "5"))

      (* a non-numeric index is likewise malformed and degrades, not raises. *)
      val () = checkNoRaise "non-numeric SST index does not raise"
        (fn () => a1Of "notanumber")
      val () = checkValue "non-numeric SST index -> empty-string fallback"
        (X.Str "", valOf (a1Of "notanumber"))
    in
      ()
    end
end
