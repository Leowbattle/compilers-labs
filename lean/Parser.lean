/-
  Direct port of `Parser.hs`.

  `Parser a` is `[Char] -> [(a, [Char])]`. Haskell's `String` is `[Char]`, so
  `Str` is the parser's input type; a source `String` is converted once in
  `parse`, and token text is converted back once when it is captured.
-/

/-- Haskell's `String`: a list of characters. -/
abbrev Str := List Char

def Parser (α : Type) : Type := Str → List (α × Str)

def parse {α : Type} (p : Parser α) (src : String) : List (α × Str) := p src.toList

instance : Functor Parser where
  map f p := fun src => (p src).map fun (v, src') => (f v, src')

instance : Monad Parser where
  pure v := fun src => [(v, src)]
  bind p f := fun src => (p src).flatMap fun (v, src') => f v src'

instance : Alternative Parser where
  failure := fun _ => []
  orElse p q := fun src =>
    match p src with
    | [] => q () src
    | rs => rs

/- Equivalent to Haskell's span -/
def splitWhile (pred : Char → Bool) (s : Str) : Str × Str :=
  (s.takeWhile pred, s.dropWhile pred)

/--
A total fixed-point combinator for parsers.

The recursive parser may only call itself on strictly shorter input; a call on
the same or a longer input returns no results. This makes recursive grammars
total without a separate fuel parameter.
-/
def parserFix {α : Type} (f : Parser α → Parser α) : Parser α
  | src =>
      f (fun s => if _h : s.length < src.length then parserFix f s else []) src
termination_by src => src.length

def item : Parser Char := fun src =>
  match src with
  | [] => []
  | c :: cs => [(c, cs)]

/-- `read` for a non-empty sequence of ASCII digits. -/
def digitsToNat (ds : Str) : Nat :=
  ds.foldl (fun n c => n * 10 + (c.toNat - '0'.toNat)) 0

def natural : Parser Int := fun src =>
  let (ds, src') := splitWhile Char.isDigit src
  if ds.isEmpty then []
  else [(Int.ofNat (digitsToNat ds), src')]

def spaces : Parser Unit := fun src =>
  [((), src.dropWhile Char.isWhitespace)]

def pFail {α : Type} : Parser α := fun _ => []

def empty {α : Type} : Parser α := failure

def satisfy {α : Type} (p : Parser α) (cond : α → Bool) : Parser α := do
  let x ← p
  if cond x then return x else pFail

def char (x : Char) : Parser Char := satisfy item (· == x)

/-- Consume the given characters; on success `string` returns the literal. -/
def stringAux : Str → Parser Unit
  | [] => pure ()
  | x :: xs => do
      let _ ← char x
      let _ ← stringAux xs
      return ()

def string (s : String) : Parser String := do
  let _ ← stringAux s.toList
  return s

/-- Case-insensitive variant of `stringAux`. -/
def caselessAux : Str → Parser Unit
  | [] => pure ()
  | x :: xs => do
      let _ ← char (Char.toLower x) <|> char (Char.toUpper x)
      let _ ← caselessAux xs
      return ()

def caseless (s : String) : Parser String := do
  let _ ← caselessAux s.toList
  return s

def token {α : Type} (p : Parser α) : Parser α := do
  spaces
  let v ← p
  spaces
  return v

/--
`many p` is total. It only recurses when `p` consumes input; if `p` succeeds
without making progress, the result list stops. This is the same behaviour for
every consuming parser used by the compiler and avoids a separate fuel
parameter.
-/
def many {α : Type} (p : Parser α) : Parser (List α) :=
  parserFix fun self => fun src =>
    match p src with
    | [] => [([], src)]
    | results =>
        results.flatMap fun (a, rest) =>
          (self rest).map fun (as, rest') => (a :: as, rest')
