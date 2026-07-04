(* xlsx.sml

   Office Open XML (`.xlsx`) reader/writer. The XML parts are built and parsed
   with the vendored `Xml`; the package is zipped/unzipped with the vendored
   `Zip`. Deterministic: ZIP mod-times are zeroed by `sml-zip` and numbers are
   formatted with a fixed, compiler-independent routine, so output is
   byte-identical across runs and across MLton / Poly/ML. *)

structure Xlsx :> XLSX =
struct
  datatype value =
      Num of real
    | Str of string
    | Bool of bool
    | Formula of string * value

  type cell = string * value
  type sheet = { name : string, cells : cell list }
  type workbook = { sheets : sheet list }

  exception Xlsx of string

  fun cell c = c
  fun sheet (name, cells) = { name = name, cells = cells }
  fun workbook sheets = { sheets = sheets }

  (* ---- namespace / content-type constants ------------------------------- *)

  val mainNs   = "http://schemas.openxmlformats.org/spreadsheetml/2006/main"
  val ctNs     = "http://schemas.openxmlformats.org/package/2006/content-types"
  val pkgRelNs = "http://schemas.openxmlformats.org/package/2006/relationships"
  val odRelNs  = "http://schemas.openxmlformats.org/officeDocument/2006/relationships"

  val xmlDecl  = "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>"

  (* ---- small helpers ----------------------------------------------------- *)

  (* map a function over a list together with each element's 0-based index *)
  fun mapi f xs =
    let
      fun go (_, []) = []
        | go (i, x :: rest) = f (i, x) :: go (i + 1, rest)
    in go (0, xs) end

  (* split an A1-style reference into (column letters, row number). *)
  fun splitRef r =
    let
      val n = String.size r
      fun loop i =
        if i < n andalso Char.isAlpha (String.sub (r, i)) then loop (i + 1) else i
      val k = loop 0
    in
      (String.substring (r, 0, k), String.extract (r, k, NONE))
    end

  fun rowNumOf r =
    (* Parse the row number via `IntInf` (never overflows) and bound to the
       portable signed-32-bit range, so a huge row in a malformed reference is
       rejected identically on MLton and Poly/ML rather than raising `Overflow`
       under MLton's 32-bit `int`. *)
    case IntInf.fromString (#2 (splitRef r)) of
        SOME n => if n >= 0 andalso n <= 2147483647 then IntInf.toInt n
                  else raise Xlsx ("cell row out of range: " ^ r)
      | NONE => raise Xlsx ("malformed cell reference: " ^ r)

  (* Deterministic decimal formatting independent of the compiler's
     Real.toString / GEN behaviour (which differs between MLton and Poly/ML):
     fixed-point with 10 fractional digits, then trailing zeros (and a bare
     decimal point) trimmed and SML's '~' sign rewritten as '-'. *)
  fun fmtNum r =
    let
      val raw = Real.fmt (StringCvt.FIX (SOME 10)) r
      val raw = String.map (fn #"~" => #"-" | c => c) raw
    in
      if String.isSubstring "." raw then
        let
          fun dropZeros (#"0" :: rest) = dropZeros rest
            | dropZeros (#"." :: rest) = rest
            | dropZeros cs = cs
        in
          String.implode (List.rev (dropZeros (List.rev (String.explode raw))))
        end
      else raw
    end

  (* ---- shared-string pool ------------------------------------------------ *)

  fun collectStrings (wb : workbook) =
    let
      fun fromCells cells =
        List.mapPartial (fn (_, Str s) => SOME s | _ => NONE) cells
      val all = List.concat (List.map (fn sh => fromCells (#cells sh)) (#sheets wb))
      fun dedup ([], acc) = List.rev acc
        | dedup (x :: xs, acc) =
            if List.exists (fn y => y = x) acc then dedup (xs, acc)
            else dedup (xs, x :: acc)
    in
      { uniq = dedup (all, []), total = List.length all }
    end

  fun poolIndex (uniq, s) =
    let
      fun loop (_, []) = raise Xlsx ("shared string not in pool: " ^ s)
        | loop (i, x :: xs) = if x = s then i else loop (i + 1, xs)
    in loop (0, uniq) end

  (* ---- XML node builders ------------------------------------------------- *)

  fun el (name, attrs, children) =
    Xml.Element { name = name, ns = NONE, attrs = attrs, children = children }
  fun text s = Xml.Text s

  fun vEl s = el ("v", [], [text s])
  fun fEl f = el ("f", [], [text f])

  fun cellNode uniq (cref, value) =
    let
      fun c (extra, children) = el ("c", ("r", cref) :: extra, children)
      fun base v =
        case v of
            Num r => ([], vEl (fmtNum r))
          | Str s => ([("t", "s")], vEl (Int.toString (poolIndex (uniq, s))))
          | Bool b => ([("t", "b")], vEl (if b then "1" else "0"))
          | Formula _ => raise Xlsx "nested formula is not supported"
    in
      case value of
          Formula (f, cached) =>
            let
              (* a formula's cached result uses an inline type, not the pool *)
              val (extra, vstr) =
                case cached of
                    Num r => ([], fmtNum r)
                  | Str s => ([("t", "str")], s)
                  | Bool b => ([("t", "b")], if b then "1" else "0")
                  | Formula _ => raise Xlsx "nested formula is not supported"
            in
              c (extra, [fEl f, vEl vstr])
            end
        | v => let val (extra, vnode) = base v in c (extra, [vnode]) end
    end

  fun rowNode uniq cells =
    let
      val rn = rowNumOf (#1 (hd cells))
    in
      el ("row", [("r", Int.toString rn)], List.map (cellNode uniq) cells)
    end

  fun groupRows cells =
    let
      val ordered =
        List.foldl
          (fn ((r, _), acc) =>
             let val rn = rowNumOf r in
               if List.exists (fn x => x = rn) acc then acc else acc @ [rn]
             end)
          [] cells
    in
      List.map
        (fn rn => List.filter (fn (r, _) => rowNumOf r = rn) cells)
        ordered
    end

  fun worksheetNode uniq (sh : sheet) =
    let
      val rows = List.map (rowNode uniq) (groupRows (#cells sh))
    in
      el ("worksheet", [("xmlns", mainNs)], [el ("sheetData", [], rows)])
    end

  fun siNode s =
    let
      (* preserve significant leading/trailing whitespace per the spec *)
      val needPreserve =
        s <> "" andalso
        (Char.isSpace (String.sub (s, 0))
         orelse Char.isSpace (String.sub (s, String.size s - 1)))
      val attrs = if needPreserve then [("xml:space", "preserve")] else []
    in
      el ("si", [], [el ("t", attrs, [text s])])
    end

  fun sharedStringsNode {uniq, total} =
    el ("sst",
        [("xmlns", mainNs),
         ("count", Int.toString total),
         ("uniqueCount", Int.toString (List.length uniq))],
        List.map siNode uniq)

  fun contentTypesNode nSheets =
    let
      fun deflt (ext, ct) =
        el ("Default", [("Extension", ext), ("ContentType", ct)], [])
      fun override (part, ct) =
        el ("Override", [("PartName", part), ("ContentType", ct)], [])
      val sheetOverrides =
        List.tabulate
          (nSheets,
           fn i =>
             override
               ("/xl/worksheets/sheet" ^ Int.toString (i + 1) ^ ".xml",
                "application/vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml"))
    in
      el ("Types", [("xmlns", ctNs)],
          [ deflt ("rels", "application/vnd.openxmlformats-package.relationships+xml"),
            deflt ("xml", "application/xml"),
            override ("/xl/workbook.xml",
                      "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet.main+xml") ]
          @ sheetOverrides
          @ [ override ("/xl/sharedStrings.xml",
                        "application/vnd.openxmlformats-officedocument.spreadsheetml.sharedStrings+xml") ])
    end

  fun rootRelsNode () =
    el ("Relationships", [("xmlns", pkgRelNs)],
        [ el ("Relationship",
              [("Id", "rId1"),
               ("Type", odRelNs ^ "/officeDocument"),
               ("Target", "xl/workbook.xml")], []) ])

  fun workbookNode (sheets : sheet list) =
    let
      val sheetEls =
        mapi
          (fn (i, sh) =>
             el ("sheet",
                 [("name", #name sh),
                  ("sheetId", Int.toString (i + 1)),
                  ("r:id", "rId" ^ Int.toString (i + 1))], []))
          sheets
    in
      el ("workbook", [("xmlns", mainNs), ("xmlns:r", odRelNs)],
          [el ("sheets", [], sheetEls)])
    end

  fun workbookRelsNode nSheets =
    let
      val sheetRels =
        List.tabulate
          (nSheets,
           fn i =>
             el ("Relationship",
                 [("Id", "rId" ^ Int.toString (i + 1)),
                  ("Type", odRelNs ^ "/worksheet"),
                  ("Target", "worksheets/sheet" ^ Int.toString (i + 1) ^ ".xml")], []))
      val ssRel =
        el ("Relationship",
            [("Id", "rId" ^ Int.toString (nSheets + 1)),
             ("Type", odRelNs ^ "/sharedStrings"),
             ("Target", "sharedStrings.xml")], [])
    in
      el ("Relationships", [("xmlns", pkgRelNs)], sheetRels @ [ssRel])
    end

  fun renderPart node = xmlDecl ^ Xml.render node

  fun parts (wb : workbook) =
    let
      val sheets = #sheets wb
      val n = List.length sheets
      val pool = collectStrings wb
      val worksheetParts =
        mapi
          (fn (i, sh) =>
             ("xl/worksheets/sheet" ^ Int.toString (i + 1) ^ ".xml",
              renderPart (worksheetNode (#uniq pool) sh)))
          sheets
    in
      [ ("[Content_Types].xml", renderPart (contentTypesNode n)),
        ("_rels/.rels", renderPart (rootRelsNode ())),
        ("xl/workbook.xml", renderPart (workbookNode sheets)),
        ("xl/_rels/workbook.xml.rels", renderPart (workbookRelsNode n)) ]
      @ worksheetParts
      @ [ ("xl/sharedStrings.xml", renderPart (sharedStringsNode pool)) ]
    end

  (* ---- serialize --------------------------------------------------------- *)

  fun toBytes wb =
    let
      val entries =
        List.map
          (fn (name, xml) => Zip.deflated (name, Byte.stringToBytes xml))
          (parts wb)
    in
      Zip.write { entries = entries, level = 6 }
    end

  (* ---- parse ------------------------------------------------------------- *)

  fun parseDoc s = Xml.parse s

  fun firstChildNamed (node, nm) =
    case Xml.byName nm node of x :: _ => SOME x | [] => NONE

  fun reqAttr (node, k) =
    case Xml.getAttr node k of
        SOME v => v
      | NONE => raise Xlsx ("missing attribute '" ^ k ^ "'")

  fun parseSst s =
    let
      val root = parseDoc s
      val sis = Xml.byName "si" root
    in
      Vector.fromList (List.map Xml.textContent sis)
    end

  fun parseNum vs =
    case Real.fromString vs of
        SOME r => r
      | NONE => raise Xlsx ("malformed number: " ^ vs)

  (* Resolve a shared-string reference from an *untrusted* worksheet cell. The
     index text comes straight from the file, so it may be non-numeric, negative,
     larger than a 32-bit `int` can hold (which would overflow `Int.fromString`
     on MLton while succeeding on Poly/ML), or an in-range integer that points
     past the end of the table. Parse with `IntInf.fromString` (never overflows)
     and bounds-check against a fixed 32-bit literal -- `Int.maxInt` is `NONE`
     under Poly/ML -- before narrowing to `int`; then bounds-check the vector.
     Any malformed or out-of-range reference degrades to the documented
     empty-string fallback (matching the `NONE => Str ""` case above), so the
     reader never raises `Overflow` / `Option` / `Subscript` and behaves
     identically across compilers. *)
  fun sstLookup (sst, vs) =
    let
      val maxInt32 : IntInf.int = 2147483647
    in
      case IntInf.fromString vs of
          SOME i =>
            if i >= 0 andalso i <= maxInt32 then
              let val idx = IntInf.toInt i in
                if idx < Vector.length sst then Vector.sub (sst, idx) else ""
              end
            else ""
        | NONE => ""
    end

  fun parseWorksheet (sst, s) =
    let
      val root = parseDoc s
      val cs = Xml.byName "c" root
      fun parseCell el0 =
        let
          val cref = reqAttr (el0, "r")
          val t = Xml.getAttr el0 "t"
          val fOpt = Option.map Xml.textContent (firstChildNamed (el0, "f"))
          val vOpt = Option.map Xml.textContent (firstChildNamed (el0, "v"))
          fun baseValue () =
            case t of
                SOME "s" =>
                  (case vOpt of
                       SOME vs => Str (sstLookup (sst, vs))
                     | NONE => Str "")
              | SOME "b" => Bool (vOpt = SOME "1")
              | SOME "str" => Str (Option.getOpt (vOpt, ""))
              | SOME "n" => Num (parseNum (Option.getOpt (vOpt, "0")))
              | NONE => Num (parseNum (Option.getOpt (vOpt, "0")))
              | SOME other => raise Xlsx ("unsupported cell type: " ^ other)
        in
          (* skip structurally empty cells (no value, no formula) *)
          case (fOpt, vOpt) of
              (NONE, NONE) => NONE
            | (SOME f, _) => SOME (cref, Formula (f, baseValue ()))
            | (NONE, _) => SOME (cref, baseValue ())
        end
    in
      List.mapPartial parseCell cs
    end

  fun fromBytes bytes =
    let
      val arc = Zip.read bytes
      fun findPart name =
        Option.map (fn e => Byte.bytesToString (#contents e)) (Zip.find arc name)
      fun part name =
        case findPart name of
            SOME s => s
          | NONE => raise Xlsx ("missing package part: " ^ name)

      val sst =
        case findPart "xl/sharedStrings.xml" of
            NONE => Vector.fromList []
          | SOME s => parseSst s

      val relsRoot = parseDoc (part "xl/_rels/workbook.xml.rels")
      val relEls = Xml.byName "Relationship" relsRoot
      fun targetOf rid =
        case List.find (fn e => Xml.getAttr e "Id" = SOME rid) relEls of
            SOME e => reqAttr (e, "Target")
          | NONE => raise Xlsx ("no relationship with Id " ^ rid)

      val wbRoot = parseDoc (part "xl/workbook.xml")
      val sheetEls = Xml.byName "sheet" wbRoot
      fun readSheet el0 =
        let
          val name = reqAttr (el0, "name")
          val rid = reqAttr (el0, "r:id")
          val target = targetOf rid
          val xml = part ("xl/" ^ target)
        in
          { name = name, cells = parseWorksheet (sst, xml) }
        end
    in
      { sheets = List.map readSheet sheetEls }
    end

  (* ---- accessors --------------------------------------------------------- *)

  fun sheetNames (wb : workbook) = List.map #name (#sheets wb)
  fun cellRefs (sh : sheet) = List.map #1 (#cells sh)
  fun getCell (sh : sheet) cref =
    Option.map #2 (List.find (fn (r, _) => r = cref) (#cells sh))
end
