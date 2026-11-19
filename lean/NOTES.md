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
- **State monads.** Removed the custom `St`/`StateIO` wrapper; the port uses
  Lean's `StateM`/`StateT` directly (`get`/`set`/`modify`, `StateT.run`,
  `StateT.lift`).
- **`Show` for stack modes.** Added `ToString` and a custom `Repr` for both
  `StackMode` and `AddressOperation`, matching Haskell's `Show`:
  `SB`/`LB`, `+`/`-`. Removed `showStackMode`/`showAddressOperation`; the
  `TAMInst` printer now uses `toString`. Kept both instances for Haskell
  closeness.
- **`operator` shape.** Rewrote `MTParser.operator` to map the operator list to
  parsers and then fold with `<|>`, matching
  `foldl (<|>) empty $ map (…) ops`.

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

## Parser representation decision

Decision: keep the parser input as `Str := List Char`. Do not migrate the
production parser to `String.Slice` or `Std.Internal.Parsec` for now.

`IntParserStyles.lean` is the supporting experiment: the same signed-integer
parser and unrestricted round-trip theorem in three styles. Nonblank line
counts (implementation / proof):

| style | implementation | proof | total |
|---|---:|---:|---:|
| `ListChar` | 31 | 52 | 83 |
| `String.Slice` | 29 | 90 | 119 |
| `Std.Internal.Parsec` | 18 | 83 | 101 |

The counts exclude 18 shared setup lines and include proof helper lemmas.
`LeanParsec` is shorter to implement because it reuses Std's parser type, but
its proof is still large because it must reason about slices, positions, and
`ParseResult` directly.

Reasons for keeping `List Char`:

- The production integer parser already uses the total `splitWhile` helper, not
  the partial `many`; the experiment now reflects that.
- `List Char` gives the shortest and most direct proof, with no `Slice.copy` or
  position reasoning.
- It keeps the existing parser close to the Haskell port and avoids mixing a
  representation migration with the remaining parser work.
- The experiment only covers integer literals. It does not measure the rest of
  the parser, where `many` and other partial functions are still used.

## Discussed, unchanged or deferred

- Switch the parser input from `Str` to `String.Slice` (O(1) slicing/positions).
- `arg0` could be `token $ caseless s *> pure i` instead of a `do` block (Lean's
  `>>` is not monadic `>>`; `*>` is the equivalent).
- Kept `Parser` as a `def` type synonym rather than a `structure`/`inductive`
  wrapper.
- Parser comparison experiment (`IntParserStyles.lean`): same signed-literal
  parser over `List Char`, `String.Slice`, and `Std.Internal.Parsec.String`,
  with a full round-trip proof for every sign and every `Nat`. Digit scanning
  is done with total loops instead of `many`; the Parsec version uses the
  low-level `Parser`/`ParseResult` interface.
- Known gaps left as-is: ASCII-only `Char` predicates vs Unicode `Data.Char`,
  64-bit `Int` vs arbitrary precision, `readInt` decimal-only, `1/0 = 0`, and
  `Fuzzer.hs` removed.
