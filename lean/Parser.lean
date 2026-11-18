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
`many p = some p <|> pure []`, where `some p = (:) <$> p <*> many p`,
exactly as in `Control.Applicative`.
-/
partial def many {α : Type} (p : Parser α) : Parser (List α) :=
  (List.cons <$> p <*> many p) <|> pure []
