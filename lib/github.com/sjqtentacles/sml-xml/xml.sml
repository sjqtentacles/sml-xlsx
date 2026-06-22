(* xml.sml

   Recursive-descent XML parser, DOM, and serializer with namespace support.
   Pure Standard ML; the only dependency is the vendored sml-unicode, used to
   encode numeric character references as UTF-8. Deterministic and identical
   across MLton and Poly/ML. *)

structure Xml :> XML =
struct
  datatype node =
      Element of { name     : string
                 , ns       : string option
                 , attrs    : (string * string) list
                 , children : node list }
    | Text of string
    | Comment of string
    | CData of string

  exception Xml of string

  fun err msg = raise Xml msg

  (* The XML 1.0 reserved namespace bound to the `xml` prefix. *)
  val xmlNsUri = "http://www.w3.org/XML/1998/namespace"

  (* ---- escaping ---------------------------------------------------------- *)

  fun escapeText s =
    String.translate
      (fn #"&" => "&amp;"
        | #"<" => "&lt;"
        | #">" => "&gt;"
        | c    => String.str c)
      s

  fun escapeAttr s =
    String.translate
      (fn #"&"  => "&amp;"
        | #"<"  => "&lt;"
        | #">"  => "&gt;"
        | #"\"" => "&quot;"
        | c     => String.str c)
      s

  (* ---- entity decoding --------------------------------------------------- *)

  (* `rest` is everything after the `#` in a character reference, e.g. "65"
     (decimal) or "x41" / "X41" (hex). Returns the UTF-8 encoding of the
     codepoint. *)
  fun parseCharRef rest =
    let
      val (isHex, ds) =
        if String.size rest >= 1
           andalso (String.sub (rest, 0) = #"x" orelse String.sub (rest, 0) = #"X")
        then (true, String.extract (rest, 1, NONE))
        else (false, rest)
      val () = if ds = "" then err "empty character reference" else ()
      val base = if isHex then 16 else 10
      fun digitVal c =
        if c >= #"0" andalso c <= #"9" then SOME (ord c - ord #"0")
        else if isHex andalso c >= #"a" andalso c <= #"f" then SOME (ord c - ord #"a" + 10)
        else if isHex andalso c >= #"A" andalso c <= #"F" then SOME (ord c - ord #"A" + 10)
        else NONE
      fun fold (c, acc) =
        case acc of
            NONE => NONE
          | SOME v => (case digitVal c of SOME d => SOME (v * base + d) | NONE => NONE)
    in
      case CharVector.foldl fold (SOME 0) ds of
          SOME cp => (Unicode.encodeUtf8 [cp]
                      handle _ => err "character reference out of range")
        | NONE => err "malformed character reference"
    end

  (* Decode the name/number between `&` and `;`. *)
  fun decodeEntity ent =
    case ent of
        "lt"   => "<"
      | "gt"   => ">"
      | "amp"  => "&"
      | "quot" => "\""
      | "apos" => "'"
      | _ =>
          if String.size ent >= 1 andalso String.sub (ent, 0) = #"#" then
            parseCharRef (String.extract (ent, 1, NONE))
          else err ("unknown entity: &" ^ ent ^ ";")

  (* Expand all entity references in a raw (text or attribute) segment. *)
  fun decodeEntities raw =
    let
      val m = String.size raw
      fun go (i, acc) =
        if i >= m then String.concat (List.rev acc)
        else
          let val c = String.sub (raw, i) in
            if c = #"&" then
              let
                fun findSemi j =
                  if j >= m then err "unterminated entity reference"
                  else if String.sub (raw, j) = #";" then j
                  else findSemi (j + 1)
                val semi = findSemi (i + 1)
                val ent = String.substring (raw, i + 1, semi - (i + 1))
              in
                go (semi + 1, decodeEntity ent :: acc)
              end
            else
              go (i + 1, String.str c :: acc)
          end
    in
      go (0, [])
    end

  (* ---- namespace scope --------------------------------------------------- *)

  type scope = { dflt : string option, prefixes : (string * string) list }
  val emptyScope : scope = { dflt = NONE, prefixes = [] }

  (* Fold an element's attributes into the in-scope namespace bindings. *)
  fun addDecls (attrs, { dflt, prefixes } : scope) : scope =
    let
      fun upd ((k, v), (d, ps)) =
        if k = "xmlns" then ((if v = "" then NONE else SOME v), ps)
        else if String.isPrefix "xmlns:" k then (d, (String.extract (k, 6, NONE), v) :: ps)
        else (d, ps)
      val (d', ps') = List.foldl upd (dflt, prefixes) attrs
    in
      { dflt = d', prefixes = ps' }
    end

  (* Split a qualified name into (prefix option, local name). *)
  fun splitName nm =
    let
      val k = String.size nm
      fun find i =
        if i >= k then NONE
        else if String.sub (nm, i) = #":" then SOME i
        else find (i + 1)
    in
      case find 0 of
          SOME i => (SOME (String.substring (nm, 0, i)), String.extract (nm, i + 1, NONE))
        | NONE => (NONE, nm)
    end

  (* Resolve a raw element name against a scope to (local name, ns URI). *)
  fun resolve (rawname, { dflt, prefixes } : scope) =
    let val (pfx, loc) = splitName rawname in
      case pfx of
          NONE => (loc, dflt)
        | SOME "xml" => (loc, SOME xmlNsUri)
        | SOME "xmlns" => (loc, NONE)
        | SOME p =>
            (case List.find (fn (k, _) => k = p) prefixes of
                 SOME (_, uri) => (loc, SOME uri)
               | NONE => err ("unbound namespace prefix: " ^ p))
    end

  (* Reconstruct a qualified name for serialization from the resolved ns and
     the in-scope declarations. *)
  fun qualify (loc, ns, { dflt, prefixes } : scope) =
    case ns of
        NONE => loc
      | SOME uri =>
          if dflt = SOME uri then loc
          else case List.find (fn (_, u) => u = uri) prefixes of
                   SOME (p, _) => p ^ ":" ^ loc
                 | NONE => if uri = xmlNsUri then "xml:" ^ loc else loc

  (* ---- parser ------------------------------------------------------------ *)

  fun parse input =
    let
      val n = String.size input
      val pos = ref 0

      fun peek () = if !pos < n then SOME (String.sub (input, !pos)) else NONE
      fun adv () = pos := !pos + 1
      fun eof () = !pos >= n
      fun looking str =
        let val k = String.size str
        in !pos + k <= n andalso String.substring (input, !pos, k) = str end

      fun isWs c = c = #" " orelse c = #"\t" orelse c = #"\n" orelse c = #"\r"
      fun skipWs () =
        case peek () of SOME c => if isWs c then (adv (); skipWs ()) else () | NONE => ()

      fun isNameChar c =
        not (isWs c) andalso c <> #"/" andalso c <> #">" andalso c <> #"<"
        andalso c <> #"=" andalso c <> #"\"" andalso c <> #"'"

      fun readName () =
        let
          val start = !pos
          fun loop () =
            case peek () of SOME c => if isNameChar c then (adv (); loop ()) else () | NONE => ()
        in
          loop ();
          if !pos = start then err "expected a name"
          else String.substring (input, start, !pos - start)
        end

      fun readAttrValue () =
        case peek () of
            SOME q =>
              if q = #"\"" orelse q = #"'" then
                let
                  val () = adv ()
                  val start = !pos
                  fun loop () =
                    case peek () of
                        SOME c => if c = q then () else (adv (); loop ())
                      | NONE => err "unterminated attribute value"
                  val () = loop ()
                  val raw = String.substring (input, start, !pos - start)
                  val () = adv ()  (* closing quote *)
                in
                  decodeEntities raw
                end
              else err "expected quote for attribute value"
          | NONE => err "expected attribute value"

      fun parseAttrs () =
        let
          fun loop acc =
            ( skipWs ()
            ; case peek () of
                  SOME c =>
                    if c = #"/" orelse c = #">" then List.rev acc
                    else
                      let
                        val k = readName ()
                        val () = skipWs ()
                        val () = (case peek () of SOME #"=" => adv ()
                                                | _ => err "expected '=' in attribute")
                        val () = skipWs ()
                        val v = readAttrValue ()
                      in
                        loop ((k, v) :: acc)
                      end
                | NONE => err "unexpected end of input inside a tag" )
        in
          loop []
        end

      fun parseComment () =
        let
          val () = pos := !pos + 4  (* "<!--" *)
          val start = !pos
          fun loop () =
            if looking "-->" then ()
            else case peek () of SOME _ => (adv (); loop ()) | NONE => err "unterminated comment"
          val () = loop ()
          val raw = String.substring (input, start, !pos - start)
          val () = pos := !pos + 3
        in
          Comment raw
        end

      fun parseCData () =
        let
          val () = pos := !pos + 9  (* "<![CDATA[" *)
          val start = !pos
          fun loop () =
            if looking "]]>" then ()
            else case peek () of SOME _ => (adv (); loop ()) | NONE => err "unterminated CDATA section"
          val () = loop ()
          val raw = String.substring (input, start, !pos - start)
          val () = pos := !pos + 3
        in
          CData raw
        end

      fun skipPI () =
        ( pos := !pos + 2  (* "<?" *)
        ; let
            fun loop () =
              if looking "?>" then pos := !pos + 2
              else case peek () of SOME _ => (adv (); loop ())
                                 | NONE => err "unterminated processing instruction"
          in loop () end )

      (* Skip a markup declaration such as <!DOCTYPE ...>, tracking [ ] depth so
         an internal subset containing '>' does not end it early. *)
      fun skipBang () =
        ( pos := !pos + 2  (* "<!" *)
        ; let
            fun loop depth =
              case peek () of
                  NONE => err "unterminated markup declaration"
                | SOME #"[" => (adv (); loop (depth + 1))
                | SOME #"]" => (adv (); loop (depth - 1))
                | SOME #">" => if depth <= 0 then adv () else (adv (); loop depth)
                | SOME _ => (adv (); loop depth)
          in loop 0 end )

      fun parseText () =
        let
          val start = !pos
          fun loop () =
            case peek () of SOME c => if c = #"<" then () else (adv (); loop ()) | NONE => ()
          val () = loop ()
          val raw = String.substring (input, start, !pos - start)
        in
          Text (decodeEntities raw)
        end

      fun parseElement scope =
        ( adv ()  (* consume '<' *)
        ; let
            val rawname = readName ()
            val attrs = parseAttrs ()
            val scope' = addDecls (attrs, scope)
            val (loc, ns) = resolve (rawname, scope')
          in
            if looking "/>" then
              ( pos := !pos + 2
              ; Element { name = loc, ns = ns, attrs = attrs, children = [] } )
            else if looking ">" then
              ( adv ()
              ; Element { name = loc, ns = ns, attrs = attrs,
                          children = parseContent (rawname, scope') } )
            else err "malformed start tag"
          end )

      and parseContent (rawname, scope) =
        let
          fun loop acc =
            if looking "</" then
              ( pos := !pos + 2
              ; let
                  val closeName = readName ()
                  val () = skipWs ()
                in
                  if closeName <> rawname then
                    err ("mismatched closing tag </" ^ closeName ^ "> for <" ^ rawname ^ ">")
                  else case peek () of
                           SOME #">" => (adv (); List.rev acc)
                         | _ => err "malformed closing tag"
                end )
            else if looking "<!--" then loop (parseComment () :: acc)
            else if looking "<![CDATA[" then loop (parseCData () :: acc)
            else if looking "<?" then (skipPI (); loop acc)
            else if looking "<!" then (skipBang (); loop acc)
            else if looking "<" then loop (parseElement scope :: acc)
            else
              (case peek () of
                   NONE => err "unexpected end of input; missing closing tag"
                 | SOME _ => loop (parseText () :: acc))
        in
          loop []
        end

      fun skipProlog () =
        ( skipWs ()
        ; if looking "<?" then (skipPI (); skipProlog ())
          else if looking "<!--" then (ignore (parseComment ()); skipProlog ())
          else if looking "<!" then (skipBang (); skipProlog ())
          else () )

      fun skipEpilog () =
        ( skipWs ()
        ; if looking "<!--" then (ignore (parseComment ()); skipEpilog ())
          else if looking "<?" then (skipPI (); skipEpilog ())
          else () )

      val () = skipProlog ()
      val root =
        if not (looking "<") orelse looking "</" orelse looking "<!" orelse looking "<?"
        then err "missing root element"
        else parseElement emptyScope
      val () = skipEpilog ()
    in
      if eof () then root else err "trailing content after root element"
    end

  fun parseOpt s = SOME (parse s) handle _ => NONE

  (* ---- serializer -------------------------------------------------------- *)

  fun render node =
    let
      fun renderAttr (k, v) = " " ^ k ^ "=\"" ^ escapeAttr v ^ "\""
      fun go (scope, node) =
        case node of
            Text s => escapeText s
          | Comment s => "<!--" ^ s ^ "-->"
          | CData s => "<![CDATA[" ^ s ^ "]]>"
          | Element { name, ns, attrs, children } =>
              let
                val scope' = addDecls (attrs, scope)
                val qname = qualify (name, ns, scope')
                val opening = "<" ^ qname ^ String.concat (List.map renderAttr attrs)
              in
                case children of
                    [] => opening ^ "/>"
                  | _ =>
                      opening ^ ">"
                      ^ String.concat (List.map (fn c => go (scope', c)) children)
                      ^ "</" ^ qname ^ ">"
              end
    in
      go (emptyScope, node)
    end

  (* ---- DOM helpers ------------------------------------------------------- *)

  fun findAll pred node =
    let
      val self = if pred node then [node] else []
      val kids =
        case node of
            Element { children, ... } => List.concat (List.map (findAll pred) children)
          | _ => []
    in
      self @ kids
    end

  fun byName nm =
    findAll (fn Element { name, ... } => name = nm | _ => false)

  fun children node =
    case node of Element { children = cs, ... } => cs | _ => []

  fun getAttr node k =
    case node of
        Element { attrs, ... } =>
          (case List.find (fn (a, _) => a = k) attrs of
               SOME (_, v) => SOME v
             | NONE => NONE)
      | _ => NONE

  fun localName node =
    case node of Element { name, ... } => SOME name | _ => NONE

  fun nsOf node =
    case node of Element { ns, ... } => ns | _ => NONE

  fun textContent node =
    case node of
        Text s => s
      | CData s => s
      | Comment _ => ""
      | Element { children, ... } => String.concat (List.map textContent children)
end
