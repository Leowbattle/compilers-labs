# ADR 0001: Keep `List Char` as the parser representation

- Status: Accepted
- Date: 2026-11-19

## Context

The Lean parser currently uses `Str := List Char`, matching the Haskell port.
Before committing to that representation, we compared the same small signed
integer parser in three styles:

- `List Char` with the list-of-results parser model
- `String.Slice` with the same model
- `Std.Internal.Parsec.String.Parser`

The experiment is in [`IntParserStyles.lean`](IntParserStyles.lean). It proves
the same unrestricted round-trip theorem for every sign and every `Nat`:

```lean
parseInt? (toString l) = some l
```

It is intentionally not part of the production Lake library.

## Evidence

Nonblank line counts, excluding 18 shared setup lines and including proof
helper lemmas:

| style | implementation | proof | total |
|---|---:|---:|---:|
| `ListChar` | 31 | 52 | 83 |
| `String.Slice` | 29 | 90 | 119 |
| `Std.Internal.Parsec` | 20 | 83 | 103 |

The `LeanParsec` implementation count includes its two `open` lines; without
them it is 18. `LeanParsec` is shorter to implement because it reuses Std's
parser type, but its proof is still large because it must reason about slices,
positions, and `ParseResult` directly.

## Decision

Keep the parser input as `Str := List Char`. Do not migrate the production
parser to `String.Slice` or `Std.Internal.Parsec` for now.

## Reasons

- The production integer parser already uses the total `splitWhile` helper, not
  the partial `many`; the experiment reflects that.
- `List Char` gives the shortest and most direct proof, with no `Slice.copy` or
  position reasoning.
- It keeps the existing parser close to the Haskell port and avoids mixing a
  representation migration with the remaining parser work.
- The experiment only covers integer literals. It does not measure the rest of
  the parser, where `many` and other partial functions are still used.

## Checking the experiment

The file imports the production `Parser` module, so run it from the Lean
project:

```sh
cd lean
~/.elan/bin/lake env lean ../adr/0001-int-parser-representation/IntParserStyles.lean
```
