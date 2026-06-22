(* unicode.sml

   Implementation of UNICODE. Pure SML, no FFI / threads / clock; all behaviour
   is a deterministic function of the input and the vendored `UnicodeData`
   tables. Bit twiddling in the codecs is done with plain integer arithmetic
   (div / mod) so the code is identical under MLton and Poly/ML. *)

structure Unicode :> UNICODE =
struct

  exception Malformed of string

  datatype endian = BE | LE
  datatype form = NFC | NFD

  (* ----------------------------------------------------------------- *)
  (* small helpers                                                      *)
  (* ----------------------------------------------------------------- *)

  fun inRange lo hi x = x >= lo andalso x <= hi

  (* association-list lookup over the (sorted-ish but unindexed) data tables *)
  fun assoc [] _ = NONE
    | assoc ((k, v) :: rest) x = if k = x then SOME v else assoc rest x

  val maxCodepoint = 0x10FFFF
  fun isSurrogate cp = inRange 0xD800 0xDFFF cp
  fun isScalar cp = cp >= 0 andalso cp <= maxCodepoint andalso not (isSurrogate cp)

  (* ----------------------------------------------------------------- *)
  (* UTF-8                                                              *)
  (* ----------------------------------------------------------------- *)

  fun bytesOf s = List.map Char.ord (String.explode s)
  fun strOf bytes = String.implode (List.map Char.chr bytes)

  fun isCont b = b >= 0x80 andalso b <= 0xBF

  fun decodeUtf8 s =
    let
      fun cont b = if isCont b then b - 0x80
                   else raise Malformed "expected continuation byte"
      fun go [] = []
        | go (b0 :: rest) =
            if b0 < 0x80 then b0 :: go rest
            else if b0 < 0xC0 then
              raise Malformed "unexpected continuation byte"
            else if b0 < 0xE0 then
              (case rest of
                 b1 :: r =>
                   let val cp = (b0 - 0xC0) * 0x40 + cont b1
                   in if cp < 0x80 then raise Malformed "overlong 2-byte sequence"
                      else cp :: go r
                   end
               | [] => raise Malformed "truncated 2-byte sequence")
            else if b0 < 0xF0 then
              (case rest of
                 b1 :: b2 :: r =>
                   let val cp = (b0 - 0xE0) * 0x1000 + cont b1 * 0x40 + cont b2
                   in if cp < 0x800 then raise Malformed "overlong 3-byte sequence"
                      else if isSurrogate cp then raise Malformed "surrogate codepoint"
                      else cp :: go r
                   end
               | _ => raise Malformed "truncated 3-byte sequence")
            else if b0 < 0xF8 then
              (case rest of
                 b1 :: b2 :: b3 :: r =>
                   let val cp = (b0 - 0xF0) * 0x40000 + cont b1 * 0x1000
                                + cont b2 * 0x40 + cont b3
                   in if cp < 0x10000 then raise Malformed "overlong 4-byte sequence"
                      else if cp > maxCodepoint then raise Malformed "codepoint out of range"
                      else cp :: go r
                   end
               | _ => raise Malformed "truncated 4-byte sequence")
            else
              raise Malformed "invalid leading byte"
    in
      go (bytesOf s)
    end

  fun encodeOne cp =
    if not (isScalar cp) then
      raise Malformed "not a Unicode scalar value"
    else if cp < 0x80 then [cp]
    else if cp < 0x800 then
      [0xC0 + cp div 0x40, 0x80 + cp mod 0x40]
    else if cp < 0x10000 then
      [0xE0 + cp div 0x1000,
       0x80 + (cp div 0x40) mod 0x40,
       0x80 + cp mod 0x40]
    else
      [0xF0 + cp div 0x40000,
       0x80 + (cp div 0x1000) mod 0x40,
       0x80 + (cp div 0x40) mod 0x40,
       0x80 + cp mod 0x40]

  fun encodeUtf8 cps = strOf (List.concat (List.map encodeOne cps))

  (* ----------------------------------------------------------------- *)
  (* UTF-16                                                             *)
  (* ----------------------------------------------------------------- *)

  fun unitsToBytes endian unit =
    let val hi = unit div 0x100 and lo = unit mod 0x100
    in case endian of BE => [hi, lo] | LE => [lo, hi] end

  fun decodeUtf16 endian s =
    let
      val bytes = bytesOf s
      fun unit (a, b) = case endian of BE => a * 0x100 + b | LE => b * 0x100 + a
      fun go [] = []
        | go [_] = raise Malformed "odd-length UTF-16 byte string"
        | go (a :: b :: rest) =
            let val u = unit (a, b)
            in
              if inRange 0xD800 0xDBFF u then
                (case rest of
                   c :: d :: rest' =>
                     let val u2 = unit (c, d)
                     in if inRange 0xDC00 0xDFFF u2 then
                          (0x10000 + (u - 0xD800) * 0x400 + (u2 - 0xDC00)) :: go rest'
                        else raise Malformed "unpaired high surrogate"
                     end
                 | _ => raise Malformed "unpaired high surrogate")
              else if inRange 0xDC00 0xDFFF u then
                raise Malformed "unpaired low surrogate"
              else
                u :: go rest
            end
    in
      go bytes
    end

  fun encodeUtf16 endian cps =
    let
      fun one cp =
        if not (isScalar cp) then raise Malformed "not a Unicode scalar value"
        else if cp < 0x10000 then unitsToBytes endian cp
        else
          let
            val v = cp - 0x10000
            val hi = 0xD800 + v div 0x400
            val lo = 0xDC00 + v mod 0x400
          in unitsToBytes endian hi @ unitsToBytes endian lo end
    in
      strOf (List.concat (List.map one cps))
    end

  (* ----------------------------------------------------------------- *)
  (* combining class / decomposition / composition                     *)
  (* ----------------------------------------------------------------- *)

  fun ccc cp = case assoc UnicodeData.combiningClass cp of SOME c => c | NONE => 0

  (* Composition table: inverse of the length-2 canonical decompositions
     whose first element is a starter. Built once at load. *)
  val compositionTable : ((int * int) * int) list =
    List.foldr
      (fn ((cp, decomp), acc) =>
         case decomp of
           [a, b] => if ccc a = 0 then ((a, b), cp) :: acc else acc
         | _ => acc)
      []
      UnicodeData.canonicalDecomp

  fun primaryComposite (a, b) =
    let
      fun look [] = NONE
        | look ((k, v) :: rest) = if k = (a, b) then SOME v else look rest
    in look compositionTable end

  (* full canonical decomposition (recursive) *)
  fun decompose cp =
    case assoc UnicodeData.canonicalDecomp cp of
      SOME ds => List.concat (List.map decompose ds)
    | NONE => [cp]

  (* canonical ordering: stable-sort each maximal run of non-starters by ccc *)
  fun stableSortByCcc xs =
    let
      (* insertion sort, stable: a mark only moves before another of strictly
         greater ccc, never past an equal one *)
      fun insert (x, []) = [x]
        | insert (x, y :: ys) =
            if ccc x < ccc y then x :: y :: ys
            else y :: insert (x, ys)
    in
      List.foldr (fn (x, acc) => insert (x, acc)) [] xs
    end

  fun canonicalOrder cps =
    let
      fun go [] = []
        | go (x :: xs) =
            if ccc x = 0 then x :: go xs
            else
              let
                fun split (acc, []) = (List.rev acc, [])
                  | split (acc, y :: ys) =
                      if ccc y > 0 then split (y :: acc, ys)
                      else (List.rev acc, y :: ys)
                val (run, rest) = split ([x], xs)
              in
                stableSortByCcc run @ go rest
              end
    in
      go cps
    end

  fun nfd cps = canonicalOrder (List.concat (List.map decompose cps))

  fun compose [] = []
    | compose (first :: rest) =
        let
          fun loop (committed, starter, pending, lastCC, []) =
                committed @ (starter :: pending)
            | loop (committed, starter, pending, lastCC, c :: cs) =
                let val cc = ccc c
                in
                  case (if lastCC = 0 orelse lastCC < cc
                        then primaryComposite (starter, c) else NONE) of
                    SOME p => loop (committed, p, pending, lastCC, cs)
                  | NONE =>
                      if cc = 0 then
                        loop (committed @ (starter :: pending), c, [], 0, cs)
                      else
                        loop (committed, starter, pending @ [c], cc, cs)
                end
        in
          loop ([], first, [], 0, rest)
        end

  fun normalize NFD cps = nfd cps
    | normalize NFC cps = compose (nfd cps)

  (* ----------------------------------------------------------------- *)
  (* case folding (simple, common-case)                                *)
  (* ----------------------------------------------------------------- *)

  fun foldOne cp =
    if inRange 0x0041 0x005A cp then cp + 0x20        (* ASCII A-Z      *)
    else if inRange 0x00C0 0x00D6 cp then cp + 0x20   (* Latin-1 A..O   *)
    else if inRange 0x00D8 0x00DE cp then cp + 0x20   (* Latin-1 O..Th  *)
    else if inRange 0x0391 0x03A1 cp then cp + 0x20   (* Greek A..R     *)
    else if inRange 0x03A3 0x03AB cp then cp + 0x20   (* Greek S..Y     *)
    else if cp = 0x03C2 then 0x03C3                   (* final sigma    *)
    else if inRange 0x0410 0x042F cp then cp + 0x20   (* Cyrillic A..Ya *)
    else if inRange 0x0400 0x040F cp then cp + 0x50   (* Cyrillic Ie..Dzhe *)
    else cp

  fun caseFold cps = List.map foldOne cps

  (* ----------------------------------------------------------------- *)
  (* width                                                              *)
  (* ----------------------------------------------------------------- *)

  fun isCombiningMark cp =
    inRange 0x0300 0x036F cp orelse   (* Combining Diacritical Marks      *)
    inRange 0x0483 0x0489 cp orelse
    inRange 0x0591 0x05BD cp orelse
    inRange 0x0610 0x061A cp orelse
    inRange 0x064B 0x065F cp orelse
    inRange 0x06D6 0x06DC cp orelse
    inRange 0x1AB0 0x1AFF cp orelse
    inRange 0x1DC0 0x1DFF cp orelse
    inRange 0x20D0 0x20FF cp orelse   (* Combining Marks for Symbols      *)
    inRange 0xFE20 0xFE2F cp          (* Combining Half Marks             *)

  fun isZeroWidth cp =
    cp = 0x200B orelse cp = 0x200C orelse cp = 0x200D orelse
    cp = 0xFEFF orelse inRange 0xFE00 0xFE0F cp   (* ZW(N)J, BOM, VS      *)

  fun isWide cp =
    inRange 0x1100 0x115F cp orelse   (* Hangul Jamo                      *)
    cp = 0x2329 orelse cp = 0x232A orelse
    inRange 0x2E80 0x303E cp orelse   (* CJK radicals .. symbols          *)
    inRange 0x3041 0x33FF cp orelse   (* Kana, CJK symbols, enclosed      *)
    inRange 0x3400 0x4DBF cp orelse   (* CJK Ext A                        *)
    inRange 0x4E00 0x9FFF cp orelse   (* CJK Unified Ideographs           *)
    inRange 0xA000 0xA4CF cp orelse   (* Yi                               *)
    inRange 0xAC00 0xD7A3 cp orelse   (* Hangul Syllables                 *)
    inRange 0xF900 0xFAFF cp orelse   (* CJK Compatibility Ideographs     *)
    inRange 0xFE10 0xFE19 cp orelse   (* Vertical forms                   *)
    inRange 0xFE30 0xFE6F cp orelse   (* CJK compatibility / small forms  *)
    inRange 0xFF00 0xFF60 cp orelse   (* Fullwidth forms                  *)
    inRange 0xFFE0 0xFFE6 cp orelse   (* Fullwidth signs                  *)
    inRange 0x1F300 0x1FAFF cp orelse (* Emoji / pictographs              *)
    inRange 0x20000 0x3FFFD cp        (* CJK Ext B and beyond             *)

  fun width cp =
    if isCombiningMark cp orelse isZeroWidth cp then 0
    else if isWide cp then 2
    else 1

  (* ----------------------------------------------------------------- *)
  (* grapheme cluster segmentation (UAX #29, common rules)              *)
  (* ----------------------------------------------------------------- *)

  datatype gbp = CR | LF | Control | Extend | ZWJ | RI
              | L | V | T | LV | LVT | ExtPict | Other

  fun isExtPict cp =
    cp = 0x00A9 orelse cp = 0x00AE orelse cp = 0x2122 orelse
    inRange 0x2190 0x21FF cp orelse
    inRange 0x2300 0x23FF cp orelse
    inRange 0x2600 0x27BF cp orelse
    inRange 0x2B00 0x2BFF cp orelse
    inRange 0x1F000 0x1FAFF cp

  fun isExtend cp =
    isCombiningMark cp orelse
    cp = 0x200C orelse                (* ZWNJ                             *)
    inRange 0xFE00 0xFE0F cp orelse   (* variation selectors              *)
    inRange 0x1F3FB 0x1F3FF cp orelse (* emoji modifiers (skin tones)     *)
    inRange 0xE0020 0xE007F cp        (* tag characters                   *)

  fun gbpOf cp =
    if cp = 0x000D then CR
    else if cp = 0x000A then LF
    else if cp = 0x200D then ZWJ
    else if inRange 0x1F1E6 0x1F1FF cp then RI
    else if cp < 0x20 orelse inRange 0x7F 0x9F cp
            orelse cp = 0x200B orelse cp = 0x2028 orelse cp = 0x2029
            orelse cp = 0xFEFF then Control
    else if isExtend cp then Extend
    else if inRange 0x1100 0x115F cp then L
    else if inRange 0x1160 0x11A7 cp then V
    else if inRange 0x11A8 0x11FF cp then T
    else if inRange 0xAC00 0xD7A3 cp then
      (if (cp - 0xAC00) mod 28 = 0 then LV else LVT)
    else if isExtPict cp then ExtPict
    else Other

  (* emoji-ZWJ tracking state for GB11 *)
  datatype estate = ENone | EPict | EPictZwj

  (* break between prev and curr, given (riCount, emojiState) of the cluster
     that prev currently belongs to *)
  fun breaks (prev, curr, ri, emoji) =
    case (prev, curr) of
      (CR, LF) => false                              (* GB3  *)
    | (Control, _) => true                           (* GB4  *)
    | (CR, _) => true                                (* GB4  *)
    | (LF, _) => true                                (* GB4  *)
    | (_, Control) => true                           (* GB5  *)
    | (_, CR) => true                                (* GB5  *)
    | (_, LF) => true                                (* GB5  *)
    | (L, L) => false                                (* GB6  *)
    | (L, V) => false
    | (L, LV) => false
    | (L, LVT) => false
    | (LV, V) => false                               (* GB7  *)
    | (LV, T) => false
    | (V, V) => false
    | (V, T) => false
    | (LVT, T) => false                              (* GB8  *)
    | (T, T) => false
    | (_, Extend) => false                           (* GB9  *)
    | (_, ZWJ) => false                              (* GB9  *)
    | (ZWJ, ExtPict) => emoji <> EPictZwj            (* GB11 *)
    | (RI, RI) => not (ri mod 2 = 1)                 (* GB12/13 *)
    | _ => true                                      (* GB999 *)

  fun graphemes s =
    let
      val cps = decodeUtf8 s
      fun catOf cp = gbpOf cp

      (* advance emoji state when curr joins (no break) or starts a cluster *)
      fun nextEmoji (emoji, cat) =
        case cat of
          ExtPict => EPict
        | Extend  => if emoji = EPict then EPict else ENone
        | ZWJ     => if emoji = EPict then EPictZwj else ENone
        | _       => ENone

      fun nextRi (ri, prevWasRi, cat, broke) =
        case cat of
          RI => if broke then 1 else (if prevWasRi then ri + 1 else 1)
        | _  => 0

      (* clusters built in reverse; each cluster is a list of cps in order *)
      fun go (acc, cur, _, _, _, _, []) =
            List.rev (List.map List.rev (cur :: acc))
        | go (acc, cur, prevCat, ri, emoji, prevWasRi, cp :: rest) =
            let val cat = catOf cp
            in
              case cur of
                [] =>
                  go (acc, [cp], cat, nextRi (ri, false, cat, true),
                      nextEmoji (ENone, cat), cat = RI, rest)
              | _ =>
                  if breaks (prevCat, cat, ri, emoji) then
                    go (cur :: acc, [cp], cat,
                        nextRi (ri, false, cat, true),
                        nextEmoji (ENone, cat), cat = RI, rest)
                  else
                    go (acc, cp :: cur, cat,
                        nextRi (ri, prevWasRi, cat, false),
                        nextEmoji (emoji, cat), cat = RI, rest)
            end

      val clusters =
        case cps of
          [] => []
        | _ => go ([], [], Other, 0, ENone, false, cps)
    in
      List.map encodeUtf8 clusters
    end

end
