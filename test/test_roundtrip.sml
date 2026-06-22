(* test_roundtrip.sml -- write -> read round-trips and write determinism. *)

structure RoundtripTests =
struct
  open Support
  structure X = Xlsx

  fun run () =
    let
      val _ = Harness.section "round-trip: sample workbook model is preserved"
      val bytes = X.toBytes Sample.wb
      val wb' = X.fromBytes bytes
      val () = Harness.check "sample workbook round-trips" (workbookEq (Sample.wb, wb'))
      val () = Harness.checkStringList "sheet names preserved"
        (["Sheet1"], X.sheetNames wb')

      val _ = Harness.section "round-trip: individual cell values"
      val sh = hd (#sheets wb')
      val () = Harness.checkStringList "cell refs preserved"
        (["A1", "B1", "A2", "B2", "B3"], X.cellRefs sh)
      val () = checkValue "A1 = Str Name" (X.Str "Name", valOf (X.getCell sh "A1"))
      val () = checkValue "B2 = Num 1.5" (X.Num 1.5, valOf (X.getCell sh "B2"))
      val () = checkValue "B3 = formula"
        (X.Formula ("B2*2", X.Num 3.0), valOf (X.getCell sh "B3"))

      val _ = Harness.section "round-trip: all value variants"
      val wb2 =
        X.workbook
          [ X.sheet
              ("Data",
               [ ("A1", X.Num 42.0),
                 ("A2", X.Num ~3.25),
                 ("A3", X.Num 0.0),
                 ("A4", X.Num 1000000.0),
                 ("B1", X.Str "hello"),
                 ("B2", X.Str "a <b> & \"c\""),   (* exercises XML escaping *)
                 ("B3", X.Str "hello"),            (* repeated -> shared pool *)
                 ("C1", X.Bool true),
                 ("C2", X.Bool false),
                 ("D1", X.Formula ("SUM(A1:A4)", X.Num 1000038.75)),
                 ("D2", X.Formula ("CONCAT(B1,B3)", X.Str "hellohello")),
                 ("D3", X.Formula ("ISBLANK(Z9)", X.Bool false)) ]),
            X.sheet
              ("Second", [ ("A1", X.Str "second-sheet"), ("A2", X.Num 7.0) ]) ]
      val wb2' = X.fromBytes (X.toBytes wb2)
      val () = Harness.check "multi-sheet workbook round-trips" (workbookEq (wb2, wb2'))
      val () = Harness.checkStringList "multi-sheet names"
        (["Data", "Second"], X.sheetNames wb2')
      val d = hd (#sheets wb2')
      val () = checkValue "escaped string preserved"
        (X.Str "a <b> & \"c\"", valOf (X.getCell d "B2"))
      val () = checkValue "string formula preserved"
        (X.Formula ("CONCAT(B1,B3)", X.Str "hellohello"), valOf (X.getCell d "D2"))
      val () = checkValue "bool formula preserved"
        (X.Formula ("ISBLANK(Z9)", X.Bool false), valOf (X.getCell d "D3"))

      val _ = Harness.section "determinism: toBytes is byte-identical across calls"
      val () = checkBytes "sample re-encodes identically"
        (bytes, X.toBytes Sample.wb)
      val () = checkBytes "multi-sheet re-encodes identically"
        (X.toBytes wb2, X.toBytes wb2)
    in
      ()
    end
end
