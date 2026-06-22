(* demo.sml -- build a small workbook, write it to bin/demo.xlsx, then read it
   back and print a deterministic report confirming every cell round-trips.
   The output is byte-identical across runs and across MLton / Poly/ML. *)

fun writeFile (path, v) =
  let val s = BinIO.openOut path
  in BinIO.output (s, v); BinIO.closeOut s end

fun pad (s, n) =
  if String.size s >= n then s
  else s ^ String.implode (List.tabulate (n - String.size s, fn _ => #" "))

val wb =
  Xlsx.workbook
    [ Xlsx.sheet
        ("Sheet1",
         [ ("A1", Xlsx.Str "Name"),
           ("B1", Xlsx.Str "Score"),
           ("A2", Xlsx.Str "alice"),
           ("B2", Xlsx.Num 1.5),
           ("A3", Xlsx.Str "bob"),
           ("B3", Xlsx.Num 2.0),
           ("B4", Xlsx.Formula ("SUM(B2:B3)", Xlsx.Num 3.5)) ]) ]

fun valStr v =
  case v of
      Xlsx.Num r => "num " ^ (let val s = Real.fmt (StringCvt.FIX (SOME 4)) r in s end)
    | Xlsx.Str s => "str \"" ^ s ^ "\""
    | Xlsx.Bool b => "bool " ^ Bool.toString b
    | Xlsx.Formula (f, _) => "formula =" ^ f

val () =
  let
    val bytes = Xlsx.toBytes wb
    val () = writeFile ("bin/demo.xlsx", bytes)

    val wb' = Xlsx.fromBytes bytes
    val sh = hd (#sheets wb')

    val () = print "Wrote bin/demo.xlsx:\n\n"
    val () = print (pad ("  cell", 8) ^ "value\n")
    val () =
      List.app
        (fn r =>
           print (pad ("  " ^ r, 8) ^ valStr (valOf (Xlsx.getCell sh r)) ^ "\n"))
        (Xlsx.cellRefs sh)
    val () =
      print ("\nsheets: " ^ String.concatWith ", " (Xlsx.sheetNames wb')
             ^ "\narchive size: " ^ Int.toString (Word8Vector.length bytes)
             ^ " bytes; round-trips byte-exact: "
             ^ (if Xlsx.toBytes wb' = bytes then "yes" else "NO") ^ "\n")
  in
    ()
  end
