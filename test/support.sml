(* support.sml -- shared helpers for the sml-xlsx test suites. *)

structure Support =
struct
  structure X = Xlsx

  (* string <-> raw byte vector (byte-preserving) *)
  fun vec s = Byte.stringToBytes s
  fun str v = Byte.bytesToString v

  (* raw bytes -> lowercase hex, for failure messages *)
  fun toHex v =
    String.concat
      (List.map
         (fn w =>
            let val n = Word8.toInt w
                fun d k = String.sub ("0123456789abcdef", k)
            in String.implode [d (n div 16), d (n mod 16)] end)
         (Word8Vector.foldr (op ::) [] v))

  (* compare two byte vectors, reporting hex on mismatch *)
  fun checkBytes name (expected, actual) =
    Harness.checkString name (toHex expected, toHex actual)

  (* `value` carries a real, so it is not an equality type; compare structurally
     with exact real equality (the test vectors are all exactly representable). *)
  fun valueEq (a : X.value, b : X.value) =
    case (a, b) of
        (X.Num x, X.Num y) => Real.== (x, y)
      | (X.Str x, X.Str y) => x = y
      | (X.Bool x, X.Bool y) => x = y
      | (X.Formula (f1, v1), X.Formula (f2, v2)) => f1 = f2 andalso valueEq (v1, v2)
      | _ => false

  fun valueToString v =
    case v of
        X.Num r => "Num " ^ Real.toString r
      | X.Str s => "Str \"" ^ s ^ "\""
      | X.Bool b => "Bool " ^ Bool.toString b
      | X.Formula (f, c) => "Formula(\"" ^ f ^ "\"," ^ valueToString c ^ ")"

  fun checkValue name (expected, actual) =
    if valueEq (expected, actual) then Harness.check name true
    else Harness.checkString name (valueToString expected, valueToString actual)

  fun cellEq ((r1, v1) : X.cell, (r2, v2) : X.cell) =
    r1 = r2 andalso valueEq (v1, v2)

  (* Cell *ordering* is not significant across a round-trip (the writer groups
     cells row-major); compare the (ref -> value) mappings instead. *)
  fun sheetEq (s1 : X.sheet, s2 : X.sheet) =
    #name s1 = #name s2
    andalso length (#cells s1) = length (#cells s2)
    andalso List.all
              (fn (r, v) =>
                 case List.find (fn (r2, _) => r2 = r) (#cells s2) of
                     SOME (_, v2) => valueEq (v, v2)
                   | NONE => false)
              (#cells s1)

  fun workbookEq (w1 : X.workbook, w2 : X.workbook) =
    length (#sheets w1) = length (#sheets w2)
    andalso ListPair.all sheetEq (#sheets w1, #sheets w2)
end
