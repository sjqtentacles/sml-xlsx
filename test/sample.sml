(* sample.sml -- the canonical workbook used across the suites and the golden
   reference XML strings it must serialize to. *)

structure Sample =
struct
  structure X = Xlsx

  (* Sheet1: a small table with header strings, a number, a repeated string,
     and a formula cell carrying a cached numeric result. *)
  val wb : X.workbook =
    X.workbook
      [ X.sheet
          ("Sheet1",
           [ ("A1", X.Str "Name"),
             ("B1", X.Str "Score"),
             ("A2", X.Str "alice"),
             ("B2", X.Num 1.5),
             ("B3", X.Formula ("B2*2", X.Num 3.0)) ]) ]

  val xmlDecl = "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>"
  val mainNs  = "http://schemas.openxmlformats.org/spreadsheetml/2006/main"

  (* Pinned golden bytes for the two data-bearing parts. *)
  val goldenSheet1 =
    xmlDecl
    ^ "<worksheet xmlns=\"" ^ mainNs ^ "\">"
    ^ "<sheetData>"
    ^ "<row r=\"1\"><c r=\"A1\" t=\"s\"><v>0</v></c><c r=\"B1\" t=\"s\"><v>1</v></c></row>"
    ^ "<row r=\"2\"><c r=\"A2\" t=\"s\"><v>2</v></c><c r=\"B2\"><v>1.5</v></c></row>"
    ^ "<row r=\"3\"><c r=\"B3\"><f>B2*2</f><v>3</v></c></row>"
    ^ "</sheetData></worksheet>"

  val goldenSharedStrings =
    xmlDecl
    ^ "<sst xmlns=\"" ^ mainNs ^ "\" count=\"3\" uniqueCount=\"3\">"
    ^ "<si><t>Name</t></si><si><t>Score</t></si><si><t>alice</t></si>"
    ^ "</sst>"
end
