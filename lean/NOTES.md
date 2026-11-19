# Lean port notes

Changes requested by the user during the port, plus manual edits made directly by
the user. The agent implemented the requested changes unless marked deferred.

## User-requested changes

- **Project setup.** Work on a new `lean` branch, with the port in a new `lean/`
  directory, as close to the Haskell as possible: no theorems and no bug fixes.
- **Literal `parseMT`.** Keep `parseMT : String → Program` with a partial match;
  do not add an `Option`/error-returning variant. (`parseMT?` was removed, and
  `compileMT` parses once.)
- **Strings / round trip.** Remove the per-character `String` ↔ `List Char`
  conversions. Added `Str := List Char`, made `Parser` operate on `Str`, and
  convert the source once in `parse`; identifiers and TAM labels convert once
  when captured. Added `digitsToNat`, `stringAux`, and `caselessAux` so
  `string`/`caseless`/`natural` do not rebuild strings through `String.ofList`.
- **`Alternative`.** Replaced the hand-written `OrElse (Parser α)` instance with
  `Alternative Parser` (`failure`/`orElse`). `<|>` now comes from Lean's derived
  `OrElse` instance, and `empty` is `failure`.
- **Fix `main`.** The user-defined `@[extern]` used for exit-on-panic could not
  be executed by `lake env lean --run`. Replaced it with
  `LEAN_ABORT_ON_PANIC=1`, set through `Std.Async.System` at startup; removed
  `supportInterpreter`. Panics now abort in both the compiled executable and the
  interpreter (SIGABRT / status 134, no output file).
- **Simplify `many`.** Replaced the inline `flatMap`/`match` with the literal
  `many p = some p <|> pure []`:
  `(List.cons <$> p <*> many p) <|> pure []`. It remains `partial` because a
  nullable `p` loops, exactly as in Haskell.
- **`StateIO` instances.** Wrote the `Functor` and `Monad` bodies with `do`
  (`let (x, s') ← st s`), matching the Haskell `StateIO` instances.
- **`Show` for stack modes.** Added `ToString` and a custom `Repr` for both
  `StackMode` and `AddressOperation`, matching Haskell's `Show`:
  `SB`/`LB`, `+`/`-`. Removed `showStackMode`/`showAddressOperation`; the
  `TAMInst` printer now uses `toString`. Kept both instances for Haskell
  closeness.
- **`operator` shape.** Rewrote `MTParser.operator` to map the operator list to
  parsers and then fold with `<|>`, matching
  `foldl (<|>) empty $ map (…) ops`.
- **Future state-monad move.** Recorded (in `State.lean` and the README) that the
  custom `St`/`StateIO` should later be replaced by Lean's `StateM`/`StateT`
  (`get`/`set`/`modify`, `MonadState`/`MonadLift`).

## User's manual edits

- **`CodeGenerator.lean`** — the two Haskell `concat <$> mapM` sites now use
  `List.flatten <$> ...`:
  - `expCode (Expr.FunctionCall …)`:
    `let argscode ← List.flatten <$> args.mapM (expCode v)`
  - `compileProgram`:
    `let funcCode ← List.flatten <$> decls.mapM (compileFunction v)`

  The agent had originally written the equivalent `xs ← mapM …; xs.flatten`
  form. Credit for the `<$>` form: the user.

*(Only manual code edits identifiable from the working tree are listed; the Lean
sources are untracked, so there is no tracked baseline to diff against.)*

## Discussed, unchanged or deferred

- Switch the parser input from `Str` to `String.Slice` (O(1) slicing/positions).
- Replace `St`/`StateIO` with `StateM`/`StateT` (see the note in `State.lean`).
- `arg0` could be `token $ caseless s *> pure i` instead of a `do` block (Lean's
  `>>` is not monadic `>>`; `*>` is the equivalent).
- Kept `Parser` as a `def` type synonym rather than a `structure`/`inductive`
  wrapper, and `StateIO` as a `def` rather than a nominal wrapper.
- Known gaps left as-is: ASCII-only `Char` predicates vs Unicode `Data.Char`,
  64-bit `Int` vs arbitrary precision, `readInt` decimal-only, `1/0 = 0`, and
  `Fuzzer.hs` removed.
