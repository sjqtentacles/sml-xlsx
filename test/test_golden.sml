(* test_golden.sml -- assert the generated worksheet and shared-strings XML
   parts are byte-identical to pinned golden strings. *)

structure GoldenTests =
struct
  open Support
  structure X = Xlsx

  fun partNamed (ps, name) =
    case List.find (fn (n, _) => n = name) ps of
        SOME (_, xml) => xml
      | NONE => "<<missing part: " ^ name ^ ">>"

  fun run () =
    let
      val ps = X.parts Sample.wb

      val _ = Harness.section "golden: package part inventory"
      val () = Harness.checkStringList "part names in fixed order"
        ([ "[Content_Types].xml",
           "_rels/.rels",
           "xl/workbook.xml",
           "xl/_rels/workbook.xml.rels",
           "xl/worksheets/sheet1.xml",
           "xl/sharedStrings.xml" ],
         List.map #1 ps)

      val _ = Harness.section "golden: xl/worksheets/sheet1.xml"
      val () = Harness.checkString "sheet1.xml matches golden"
        (Sample.goldenSheet1, partNamed (ps, "xl/worksheets/sheet1.xml"))

      val _ = Harness.section "golden: xl/sharedStrings.xml"
      val () = Harness.checkString "sharedStrings.xml matches golden"
        (Sample.goldenSharedStrings, partNamed (ps, "xl/sharedStrings.xml"))

      val _ = Harness.section "golden: every part is well-formed XML"
      val declLen = String.size Sample.xmlDecl
      val () =
        List.app
          (fn (n, xml) =>
             Harness.check (n ^ " re-parses")
               (let
                  (* strip the leading XML declaration; the parser skips a
                     prolog but we feed it the element body directly *)
                  val body =
                    if String.isPrefix Sample.xmlDecl xml
                    then String.extract (xml, declLen, NONE)
                    else xml
                in (ignore (Xml.parse body); true) handle _ => false end))
          ps
    in
      ()
    end
end
