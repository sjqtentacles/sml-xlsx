(* data.sml

   A CURATED SUBSET of the Unicode Character Database, vendored as plain SML
   tables (no external files, no codegen at build time). These power the
   normalization, combining-class, and composition logic in `Unicode`.

   The tables are intentionally small association lists rather than one giant
   array literal: that keeps Poly/ML's compiler comfortable (it is sensitive
   to very large single literals) and keeps the data auditable. See the
   README's "Scope of Unicode data shipped" section for exact coverage.

   Data shipped here:
     * canonicalDecomp  -- canonical decomposition mappings (Decomposition_Type
                           = Canonical only; compatibility decompositions are
                           deliberately excluded).
     * combiningClass   -- Canonical_Combining_Class for the marks we cover.

   The NFC composition table is derived from `canonicalDecomp` at load time in
   `unicode.sml` (inverse of the length-2 canonical decompositions whose first
   element is a starter), so the two never drift apart. None of the codepoints
   below are on the Unicode composition-exclusion list, so the inverse mapping
   is a valid set of primary composites. *)

structure UnicodeData =
struct

  (* (precomposed, canonical decomposition).
     Latin-1 Supplement + a sampling of Latin Extended-A, all of the form
     <base letter> <combining mark>. *)
  val canonicalDecomp : (int * int list) list =
    [ (* Latin-1 Supplement, uppercase *)
      (0x00C0, [0x0041, 0x0300]),  (* A grave   *)
      (0x00C1, [0x0041, 0x0301]),  (* A acute   *)
      (0x00C2, [0x0041, 0x0302]),  (* A circ    *)
      (0x00C3, [0x0041, 0x0303]),  (* A tilde   *)
      (0x00C4, [0x0041, 0x0308]),  (* A diaer   *)
      (0x00C5, [0x0041, 0x030A]),  (* A ring    *)
      (0x00C7, [0x0043, 0x0327]),  (* C cedilla *)
      (0x00C8, [0x0045, 0x0300]),  (* E grave   *)
      (0x00C9, [0x0045, 0x0301]),  (* E acute   *)
      (0x00CA, [0x0045, 0x0302]),  (* E circ    *)
      (0x00CB, [0x0045, 0x0308]),  (* E diaer   *)
      (0x00CC, [0x0049, 0x0300]),  (* I grave   *)
      (0x00CD, [0x0049, 0x0301]),  (* I acute   *)
      (0x00CE, [0x0049, 0x0302]),  (* I circ    *)
      (0x00CF, [0x0049, 0x0308]),  (* I diaer   *)
      (0x00D1, [0x004E, 0x0303]),  (* N tilde   *)
      (0x00D2, [0x004F, 0x0300]),  (* O grave   *)
      (0x00D3, [0x004F, 0x0301]),  (* O acute   *)
      (0x00D4, [0x004F, 0x0302]),  (* O circ    *)
      (0x00D5, [0x004F, 0x0303]),  (* O tilde   *)
      (0x00D6, [0x004F, 0x0308]),  (* O diaer   *)
      (0x00D9, [0x0055, 0x0300]),  (* U grave   *)
      (0x00DA, [0x0055, 0x0301]),  (* U acute   *)
      (0x00DB, [0x0055, 0x0302]),  (* U circ    *)
      (0x00DC, [0x0055, 0x0308]),  (* U diaer   *)
      (0x00DD, [0x0059, 0x0301]),  (* Y acute   *)
      (* Latin-1 Supplement, lowercase *)
      (0x00E0, [0x0061, 0x0300]),  (* a grave   *)
      (0x00E1, [0x0061, 0x0301]),  (* a acute   *)
      (0x00E2, [0x0061, 0x0302]),  (* a circ    *)
      (0x00E3, [0x0061, 0x0303]),  (* a tilde   *)
      (0x00E4, [0x0061, 0x0308]),  (* a diaer   *)
      (0x00E5, [0x0061, 0x030A]),  (* a ring    *)
      (0x00E7, [0x0063, 0x0327]),  (* c cedilla *)
      (0x00E8, [0x0065, 0x0300]),  (* e grave   *)
      (0x00E9, [0x0065, 0x0301]),  (* e acute   *)
      (0x00EA, [0x0065, 0x0302]),  (* e circ    *)
      (0x00EB, [0x0065, 0x0308]),  (* e diaer   *)
      (0x00EC, [0x0069, 0x0300]),  (* i grave   *)
      (0x00ED, [0x0069, 0x0301]),  (* i acute   *)
      (0x00EE, [0x0069, 0x0302]),  (* i circ    *)
      (0x00EF, [0x0069, 0x0308]),  (* i diaer   *)
      (0x00F1, [0x006E, 0x0303]),  (* n tilde   *)
      (0x00F2, [0x006F, 0x0300]),  (* o grave   *)
      (0x00F3, [0x006F, 0x0301]),  (* o acute   *)
      (0x00F4, [0x006F, 0x0302]),  (* o circ    *)
      (0x00F5, [0x006F, 0x0303]),  (* o tilde   *)
      (0x00F6, [0x006F, 0x0308]),  (* o diaer   *)
      (0x00F9, [0x0075, 0x0300]),  (* u grave   *)
      (0x00FA, [0x0075, 0x0301]),  (* u acute   *)
      (0x00FB, [0x0075, 0x0302]),  (* u circ    *)
      (0x00FC, [0x0075, 0x0308]),  (* u diaer   *)
      (0x00FD, [0x0079, 0x0301]),  (* y acute   *)
      (0x00FF, [0x0079, 0x0308]),  (* y diaer   *)
      (* a sampling of Latin Extended-A (macron / tilde) *)
      (0x0100, [0x0041, 0x0304]),  (* A macron  *)
      (0x0101, [0x0061, 0x0304]),  (* a macron  *)
      (0x0112, [0x0045, 0x0304]),  (* E macron  *)
      (0x0113, [0x0065, 0x0304]),  (* e macron  *)
      (0x0128, [0x0049, 0x0303]),  (* I tilde   *)
      (0x0129, [0x0069, 0x0303]),  (* i tilde   *)
      (0x014C, [0x004F, 0x0304]),  (* O macron  *)
      (0x014D, [0x006F, 0x0304]),  (* o macron  *)
      (0x016A, [0x0055, 0x0304]),  (* U macron  *)
      (0x016B, [0x0075, 0x0304])   (* u macron  *)
    ]

  (* (codepoint, Canonical_Combining_Class) for the combining marks we cover.
     Marks not listed default to class 0 (treated as a starter). Classes:
       230 = Above, 220 = Below, 202 = Attached Below. *)
  val combiningClass : (int * int) list =
    [ (0x0300, 230), (0x0301, 230), (0x0302, 230), (0x0303, 230),
      (0x0304, 230), (0x0305, 230), (0x0306, 230), (0x0307, 230),
      (0x0308, 230), (0x0309, 230), (0x030A, 230), (0x030B, 230),
      (0x030C, 230), (0x030D, 230), (0x030E, 230), (0x030F, 230),
      (0x0310, 230), (0x0311, 230), (0x0312, 230),
      (0x0313, 230), (0x0314, 230),
      (0x0316, 220), (0x0317, 220), (0x0318, 220), (0x0319, 220),
      (0x031C, 220), (0x031D, 220), (0x031E, 220), (0x031F, 220),
      (0x0320, 220),
      (0x0323, 220), (0x0324, 220), (0x0325, 220), (0x0326, 220),
      (0x0327, 202), (0x0328, 202),
      (0x0329, 220), (0x032A, 220), (0x032B, 220),
      (0x0331, 220), (0x0332, 220), (0x0333, 220),
      (0x0334, 1),   (0x0335, 1),   (0x0336, 1),
      (0x0338, 1),
      (0x0345, 240) ]

end
