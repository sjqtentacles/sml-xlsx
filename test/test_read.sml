(* test_read.sml -- the serialized .xlsx is a real ZIP whose members are the
   expected XML parts, and reading rejects malformed input. *)

structure ReadTests =
struct
  open Support
  structure X = Xlsx

  fun run () =
    let
      val bytes = X.toBytes Sample.wb

      val _ = Harness.section "package is a valid ZIP container"
      val arc = Zip.read bytes
      val () = Harness.checkStringList "zip member names"
        ([ "[Content_Types].xml",
           "_rels/.rels",
           "xl/workbook.xml",
           "xl/_rels/workbook.xml.rels",
           "xl/worksheets/sheet1.xml",
           "xl/sharedStrings.xml" ],
         Zip.names arc)
      val () = Harness.check "begins with PK local-file-header signature"
        (Word8Vector.length bytes >= 4
         andalso Word8Vector.sub (bytes, 0) = 0w80   (* 'P' *)
         andalso Word8Vector.sub (bytes, 1) = 0w75)  (* 'K' *)

      val _ = Harness.section "the stored part bytes match parts/0"
      val ps = X.parts Sample.wb
      fun memberStr name = str (#contents (valOf (Zip.find arc name)))
      val () =
        List.app
          (fn (name, xml) =>
             Harness.checkString ("member " ^ name ^ " matches parts")
               (xml, memberStr name))
          ps

      val _ = Harness.section "reading rejects malformed input"
      val () = Harness.checkRaises "fromBytes on non-zip raises"
        (fn () => X.fromBytes (vec "this is not a zip file"))
      val () = Harness.checkRaises "fromBytes on empty input raises"
        (fn () => X.fromBytes (vec ""))
    in
      ()
    end
end
