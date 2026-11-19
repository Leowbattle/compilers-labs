/-
  The same signed-integer parser in three styles, with a full round-trip proof.

      integer literal := ['+' | '-'] digit+

  The three styles are:

    1. `ListChar`   -- the list-of-results model over `List Char`
    2. `Slice`      -- the same model over `String.Slice`
    3. `LeanParsec` -- `Std.Internal.Parsec.String.Parser`

  None of the parsers uses `many`.  Digit scanning is a total, structurally
  recursive loop in each style, so the round-trip theorem is proved for every
  `Lit` (every sign and every natural number), not by a finite test.

  `LeanParsec` uses the `Parser`/`ParseResult` interface directly.  That keeps
  the parser in the Parsec type while avoiding the partial `many`/`manyCore`
  combinators; the low-level shape mirrors `Std.Internal.Parsec.String.digitsCore`.
-/

import Parser
import Std.Internal.Parsec.String
import Init.Data.String.Lemmas

namespace IntParserStyles

/-- A signed literal: `(isNegative, absolute value)`. -/
abbrev Lit := Bool × Nat

/-- Canonical rendering: `-` for negatives, decimal digits, no leading zero. -/
def Lit.toString (l : Lit) : String :=
  String.ofList (if l.1 then '-' :: Nat.toDigits 10 l.2 else Nat.toDigits 10 l.2)

def digitsToNat (ds : List Char) : Nat := Nat.ofDigitChars 10 ds 0

theorem digitsToNat_toDigits (n : Nat) : digitsToNat (Nat.toDigits 10 n) = n :=
  Nat.ofDigitChars_ten_toDigits

theorem digits_all (n : Nat) : ∀ c ∈ Nat.toDigits 10 n, c.isDigit = true := by
  intro c hc
  exact Nat.isDigit_of_mem_toDigits (b := 10) (n := n) (by decide) (by decide) hc

theorem not_dash_of_digit {c : Char} (h : c.isDigit = true) : c ≠ '-' := by
  intro hc
  subst hc
  simp at h

theorem not_plus_of_digit {c : Char} (h : c.isDigit = true) : c ≠ '+' := by
  intro hc
  subst hc
  simp at h

namespace ListChar

abbrev Parser (α : Type) := List Char → List (α × List Char)

instance : Monad Parser where
  pure a := fun s => [(a, s)]
  bind p f := fun s => (p s).flatMap fun (a, s') => f a s'

instance : Alternative Parser where
  failure := fun _ => []
  orElse p q := fun s =>
    match p s with
    | [] => q () s
    | rs => rs

theorem alt_def {α : Type} (p q : Parser α) :
    (p <|> q) = fun s =>
      match p s with
      | [] => q s
      | rs => rs := rfl

def char (c : Char) : Parser Char := fun s =>
  match s with
  | x :: xs => if x = c then [(x, xs)] else []
  | [] => []

def splitDigits (s : List Char) : List Char × List Char :=
  splitWhile Char.isDigit s

def digits : Parser (List Char) := fun s =>
  let (ds, rest) := splitDigits s
  if ds.isEmpty then [] else [(ds, rest)]

def sign : Parser Bool :=
  (do let _ ← char '-'; return true) <|>
  (do let _ ← char '+'; return false) <|>
  return false

def parseInt : Parser Lit := do
  let neg ← sign
  let ds ← digits
  return (neg, digitsToNat ds)

def parseInt? (s : String) : Option Lit :=
  match parseInt s.toList with
  | (l, []) :: _ => some l
  | _ => none

theorem takeWhile_eq_self_of_all {ds : List Char}
    (h : ∀ c ∈ ds, c.isDigit = true) : ds.takeWhile Char.isDigit = ds := by
  induction ds with
  | nil => rfl
  | cons c cs ih =>
      have hc : c.isDigit = true := h c (by simp)
      simp [List.takeWhile, hc, ih (by intro x hx; exact h x (by simp [hx]))]

theorem dropWhile_eq_nil_of_all {ds : List Char}
    (h : ∀ c ∈ ds, c.isDigit = true) : ds.dropWhile Char.isDigit = [] := by
  induction ds with
  | nil => rfl
  | cons c cs ih =>
      have hc : c.isDigit = true := h c (by simp)
      simp [List.dropWhile, hc, ih (by intro x hx; exact h x (by simp [hx]))]

theorem splitDigits_eq_of_all {ds : List Char}
    (h : ∀ c ∈ ds, c.isDigit = true) : splitDigits ds = (ds, []) := by
  simp [splitDigits, splitWhile, takeWhile_eq_self_of_all h, dropWhile_eq_nil_of_all h]

theorem digits_eq_of_all {ds : List Char} (h : ∀ c ∈ ds, c.isDigit = true)
    (hne : ds ≠ []) : digits ds = [(ds, [])] := by
  unfold digits
  rw [splitDigits_eq_of_all h]
  cases ds with
  | nil => contradiction
  | cons c cs => simp

theorem sign_cons_digit {c : Char} {cs : List Char} (hc : c.isDigit = true) :
    sign (c :: cs) = [(false, c :: cs)] := by
  have hd := not_dash_of_digit hc
  have hp := not_plus_of_digit hc
  simp [sign, char, hd, hp, alt_def, Bind.bind, Pure.pure]

theorem sign_neg (ds : List Char) : sign ('-' :: ds) = [(true, ds)] := by
  simp [sign, char, alt_def, Bind.bind, Pure.pure]

theorem parseInt_digits {ds : List Char} (h : ∀ c ∈ ds, c.isDigit = true)
    (hne : ds ≠ []) : parseInt ds = [((false, digitsToNat ds), [])] := by
  rcases ds with _ | ⟨c, cs⟩
  · contradiction
  · have hc : c.isDigit = true := h c (by simp)
    simp [parseInt, sign_cons_digit hc, digits_eq_of_all h (by simp), Bind.bind, Pure.pure]

theorem parseInt_neg {ds : List Char} (h : ∀ c ∈ ds, c.isDigit = true)
    (hne : ds ≠ []) : parseInt ('-' :: ds) = [((true, digitsToNat ds), [])] := by
  simp [parseInt, sign_neg, digits_eq_of_all h hne, Bind.bind, Pure.pure]

theorem roundtrip (l : Lit) : parseInt? (Lit.toString l) = some l := by
  rcases l with ⟨b, n⟩
  have h := digits_all n
  have hne : Nat.toDigits 10 n ≠ [] := Nat.toDigits_ne_nil
  cases b
  · simp [Lit.toString, parseInt?, parseInt_digits h hne, digitsToNat_toDigits]
  · simp [Lit.toString, parseInt?, parseInt_neg h hne, digitsToNat_toDigits]

end ListChar

namespace Slice

abbrev Parser (α : Type) := String.Slice → List (α × String.Slice)

instance : Monad Parser where
  pure a := fun s => [(a, s)]
  bind p f := fun s => (p s).flatMap fun (a, s') => f a s'

instance : Alternative Parser where
  failure := fun _ => []
  orElse p q := fun s =>
    match p s with
    | [] => q () s
    | rs => rs

theorem alt_def {α : Type} (p q : Parser α) :
    (p <|> q) = fun s =>
      match p s with
      | [] => q s
      | rs => rs := rfl

def sign : Parser Bool := fun s =>
  match s.front? with
  | some '-' => [(true, s.drop 1)]
  | some '+' => [(false, s.drop 1)]
  | _ => [(false, s)]

def splitDigits (s : String.Slice) : List Char × String.Slice :=
  let ds := s.takeWhile Char.isDigit
  (ds.copy.toList, s.dropWhile Char.isDigit)

def digits : Parser (List Char) := fun s =>
  let (ds, rest) := splitDigits s
  if ds.isEmpty then [] else [(ds, rest)]

def parseInt : Parser Lit := do
  let neg ← sign
  let ds ← digits
  return (neg, digitsToNat ds)

def parseInt? (s : String) : Option Lit :=
  match parseInt s.toSlice with
  | (l, rest) :: _ => if rest.isEmpty then some l else none
  | [] => none

theorem all_of_copy {s : String.Slice} {ds : List Char}
    (hc : s.copy.toList = ds) (h : ∀ c ∈ ds, c.isDigit = true) :
    s.all Char.isDigit = true := by
  rw [String.Slice.all_bool_eq, hc, List.all_eq_true]
  exact h

theorem splitDigits_eq_of_copy {s : String.Slice} {ds : List Char}
    (hc : s.copy.toList = ds) (h : ∀ c ∈ ds, c.isDigit = true) :
    splitDigits s = (ds, s.dropWhile Char.isDigit) := by
  have hall := all_of_copy hc h
  unfold splitDigits
  rw [String.Slice.takeWhile_eq_self_iff.mpr hall]
  dsimp only
  rw [hc]

theorem dropWhile_empty_of_copy {s : String.Slice} {ds : List Char}
    (hc : s.copy.toList = ds) (h : ∀ c ∈ ds, c.isDigit = true) :
    (s.dropWhile Char.isDigit).isEmpty = true := by
  rw [String.Slice.isEmpty_dropWhile]
  exact all_of_copy hc h

theorem digits_eq_of_copy {s : String.Slice} {ds : List Char}
    (hc : s.copy.toList = ds) (h : ∀ c ∈ ds, c.isDigit = true)
    (hne : ds ≠ []) : digits s = [(ds, s.dropWhile Char.isDigit)] := by
  unfold digits
  rw [splitDigits_eq_of_copy hc h]
  cases ds with
  | nil => contradiction
  | cons c cs => simp

theorem sign_of_digits {s : String.Slice} {ds : List Char}
    (hc : s.copy.toList = ds) (h : ∀ c ∈ ds, c.isDigit = true)
    (hne : ds ≠ []) : sign s = [(false, s)] := by
  cases ds with
  | nil => contradiction
  | cons c cs =>
      have hc' : c.isDigit = true := h c (by simp)
      have hd := not_dash_of_digit hc'
      have hp := not_plus_of_digit hc'
      have hf : s.front? = some c := by
        rw [String.Slice.front?_eq, hc]
        rfl
      simp [sign, hf, hd, hp]

theorem sign_neg {s : String.Slice} {ds : List Char}
    (hc : s.copy.toList = '-' :: ds) : sign s = [(true, s.drop 1)] := by
  have hf : s.front? = some '-' := by
    rw [String.Slice.front?_eq, hc]
    rfl
  simp [sign, hf]

theorem parseInt_digits {s : String.Slice} {ds : List Char}
    (hc : s.copy.toList = ds) (h : ∀ c ∈ ds, c.isDigit = true)
    (hne : ds ≠ []) :
    parseInt s = [((false, digitsToNat ds), s.dropWhile Char.isDigit)] := by
  simp [parseInt, sign_of_digits hc h hne, digits_eq_of_copy hc h hne, Bind.bind, Pure.pure]

theorem parseInt_neg {s : String.Slice} {ds : List Char}
    (hc : s.copy.toList = '-' :: ds) (hc1 : (s.drop 1).copy.toList = ds)
    (h : ∀ c ∈ ds, c.isDigit = true) (hne : ds ≠ []) :
    parseInt s = [((true, digitsToNat ds), (s.drop 1).dropWhile Char.isDigit)] := by
  simp [parseInt, sign_neg hc, digits_eq_of_copy hc1 h hne, Bind.bind, Pure.pure]

theorem roundtrip (l : Lit) : parseInt? (Lit.toString l) = some l := by
  rcases l with ⟨b, n⟩
  have h := digits_all n
  have hne : Nat.toDigits 10 n ≠ [] := Nat.toDigits_ne_nil
  cases b
  · let s := (String.ofList (Nat.toDigits 10 n)).toSlice
    have hc : s.copy.toList = Nat.toDigits 10 n := by
      dsimp [s]
      rw [String.copy_toSlice, String.toList_ofList]
    have hparse : parseInt (String.ofList (Nat.toDigits 10 n)).toSlice =
        [((false, digitsToNat (Nat.toDigits 10 n)),
          (String.ofList (Nat.toDigits 10 n)).toSlice.dropWhile Char.isDigit)] := by
      simpa [s] using parseInt_digits (s := s) hc h hne
    simp [Lit.toString, parseInt?, hparse, digitsToNat_toDigits]
    exact h
  · let s := (String.ofList ('-' :: Nat.toDigits 10 n)).toSlice
    have hc : s.copy.toList = '-' :: Nat.toDigits 10 n := by
      dsimp [s]
      rw [String.copy_toSlice, String.toList_ofList]
    let t := s.drop 1
    have hc1 : t.copy.toList = Nat.toDigits 10 n := by
      dsimp [t]
      rw [String.Slice.toList_copy_drop, String.copy_toSlice, String.toList_ofList]
      rfl
    have hparse : parseInt (("-" ++ String.ofList (Nat.toDigits 10 n)).toSlice) =
        [((true, digitsToNat (Nat.toDigits 10 n)),
          (("-" ++ String.ofList (Nat.toDigits 10 n)).toSlice.drop 1).dropWhile Char.isDigit)] := by
      simpa [s, String.ofList_cons] using parseInt_neg (s := s) hc hc1 h hne
    simp [Lit.toString, parseInt?, hparse, digitsToNat_toDigits]
    exact h

end Slice

namespace LeanParsec

open Std.Internal.Parsec
open Std.Internal.Parsec.String

def parseIntCore (it : Sigma String.Pos) (neg : Bool) (t : String.Slice) :
    ParseResult Lit (Sigma String.Pos) :=
  let ds := t.takeWhile Char.isDigit
  let rest := t.dropWhile Char.isDigit
  if t.copy.toList.isEmpty then .error it (.other "expected digits")
  else if rest.isEmpty then
    .success ⟨t.str, rest.startInclusive⟩ (neg, digitsToNat ds.copy.toList)
  else .error it (.other "trailing input")

def parseInt : Std.Internal.Parsec.String.Parser Lit := fun it =>
  let s := it.1.sliceFrom it.2
  match s.front? with
  | some '-' => parseIntCore it true (s.drop 1)
  | some '+' => parseIntCore it false (s.drop 1)
  | _ => parseIntCore it false s

def parseInt? (s : String) : Option Lit :=
  match Parser.run parseInt s with
  | .ok l => some l
  | .error _ => none

theorem all_of_copy {s : String.Slice} {ds : List Char}
    (hc : s.copy.toList = ds) (h : ∀ c ∈ ds, c.isDigit = true) :
    s.all Char.isDigit = true := by
  rw [String.Slice.all_bool_eq, hc, List.all_eq_true]
  exact h

theorem parseIntCore_digits {it : Sigma String.Pos} {t : String.Slice} {ds : List Char}
    (hc : t.copy.toList = ds) (h : ∀ c ∈ ds, c.isDigit = true) (hne : ds ≠ []) :
    parseIntCore it false t =
      .success ⟨t.str, (t.dropWhile Char.isDigit).startInclusive⟩ (false, digitsToNat ds) := by
  have hall := all_of_copy hc h
  have ht : t.takeWhile Char.isDigit = t := String.Slice.takeWhile_eq_self_iff.mpr hall
  have hnonempty : t.copy.toList.isEmpty = false := by
    rw [hc]
    cases ds with
    | nil => contradiction
    | cons c cs => simp
  have hrest : (t.dropWhile Char.isDigit).isEmpty = true := by
    rw [String.Slice.isEmpty_dropWhile]
    exact hall
  simp [parseIntCore, ht, hnonempty, hrest]
  rw [hc]

theorem parseIntCore_neg {it : Sigma String.Pos} {t : String.Slice} {ds : List Char}
    (hc : t.copy.toList = ds) (h : ∀ c ∈ ds, c.isDigit = true) (hne : ds ≠ []) :
    parseIntCore it true t =
      .success ⟨t.str, (t.dropWhile Char.isDigit).startInclusive⟩ (true, digitsToNat ds) := by
  have hall := all_of_copy hc h
  have ht : t.takeWhile Char.isDigit = t := String.Slice.takeWhile_eq_self_iff.mpr hall
  have hnonempty : t.copy.toList.isEmpty = false := by
    rw [hc]
    cases ds with
    | nil => contradiction
    | cons c cs => simp
  have hrest : (t.dropWhile Char.isDigit).isEmpty = true := by
    rw [String.Slice.isEmpty_dropWhile]
    exact hall
  simp [parseIntCore, ht, hnonempty, hrest]
  rw [hc]

theorem parseInt_positive {s : String} {ds : List Char}
    (hc : s.toSlice.copy.toList = ds) (h : ∀ c ∈ ds, c.isDigit = true)
    (hne : ds ≠ []) :
    parseInt ⟨s, s.startPos⟩ =
      .success ⟨s.toSlice.str, (s.toSlice.dropWhile Char.isDigit).startInclusive⟩ (false, digitsToNat ds) := by
  cases ds with
  | nil => contradiction
  | cons c cs =>
      have hc' : c.isDigit = true := h c (by simp)
      have hd := not_dash_of_digit hc'
      have hp := not_plus_of_digit hc'
      have hshead : s.toList.head? = some c := by
        have : (s.toSlice.copy.toList).head? = some c := by rw [hc]; rfl
        simpa [String.copy_toSlice] using this
      simp [parseInt, String.Slice.front?_eq, hshead, hd, hp]
      exact parseIntCore_digits (it := ⟨s, s.startPos⟩) (t := s.toSlice) (ds := c :: cs) hc h (by simp)

theorem parseInt_negative {s : String} {ds : List Char}
    (hc : s.toSlice.copy.toList = '-' :: ds) (h : ∀ c ∈ ds, c.isDigit = true)
    (hne : ds ≠ []) :
    parseInt ⟨s, s.startPos⟩ =
      .success ⟨(s.toSlice.drop 1).str, ((s.toSlice.drop 1).dropWhile Char.isDigit).startInclusive⟩ (true, digitsToNat ds) := by
  have hshead : s.toList.head? = some '-' := by
    have : (s.toSlice.copy.toList).head? = some '-' := by rw [hc]; rfl
    simpa [String.copy_toSlice] using this
  have hc1 : (s.toSlice.drop 1).copy.toList = ds := by
    rw [String.Slice.toList_copy_drop, hc]
    rfl
  simp [parseInt, String.Slice.front?_eq, hshead]
  exact parseIntCore_neg (it := ⟨s, s.startPos⟩) (t := s.toSlice.drop 1) (ds := ds) hc1 h hne

theorem roundtrip (l : Lit) : parseInt? (Lit.toString l) = some l := by
  rcases l with ⟨b, n⟩
  have h := digits_all n
  have hne : Nat.toDigits 10 n ≠ [] := Nat.toDigits_ne_nil
  cases b
  · let s := String.ofList (Nat.toDigits 10 n)
    have hc : s.toSlice.copy.toList = Nat.toDigits 10 n := by
      dsimp [s]
      rw [String.copy_toSlice, String.toList_ofList]
    have hparse := parseInt_positive (s := s) hc h hne
    simp [Lit.toString, parseInt?, Parser.run, s, hparse, digitsToNat_toDigits]
  · let s := String.ofList ('-' :: Nat.toDigits 10 n)
    have hc : s.toSlice.copy.toList = '-' :: Nat.toDigits 10 n := by
      dsimp [s]
      rw [String.copy_toSlice, String.toList_ofList]
    have hparse := parseInt_negative (s := s) hc h hne
    simp [Lit.toString, parseInt?, Parser.run, s, hparse, digitsToNat_toDigits]

end LeanParsec

/-! ## The same theorem in all three styles -/

theorem listChar_roundtrip (l : Lit) :
    ListChar.parseInt? (Lit.toString l) = some l :=
  ListChar.roundtrip l

theorem slice_roundtrip (l : Lit) :
    Slice.parseInt? (Lit.toString l) = some l :=
  Slice.roundtrip l

theorem leanParsec_roundtrip (l : Lit) :
    LeanParsec.parseInt? (Lit.toString l) = some l :=
  LeanParsec.roundtrip l

end IntParserStyles
