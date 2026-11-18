# MT compiler in Lean 4

A direct port of the Haskell `cw2` compiler that lives in the repository root.
This is a **port only**: there are no proofs, no refactors for totality, and no
bug fixes. The goal is to reproduce the Haskell behaviour module for module.

## Build and run

```sh
lake build
lake exe mtc path/to/program.mt          # compile to path/to/program.tam
lake exe mtc path/to/program.tam         # run the TAM interpreter
lake exe mtc path/to/program.tam --trace # run with a trace
```

The toolchain is pinned in `lean-toolchain` (Lean 4.36.0). No dependencies and
no Mathlib.

## Module map

| Haskell | Lean |
|---|---|
| `Parser.hs` | `Parser.lean` |
| `State.hs` | `State.lean` |
| `MTParser.hs` | `MTParser.lean` |
| `TAMParser.hs` | `TAMParser.lean` |
| `TypeChecker.hs` | `TypeChecker.lean` |
| `CodeGenerator.hs` | `CodeGenerator.lean` |
| `TAMVirtualMachine.hs` | `TAMVirtualMachine.lean` |
| `Main.hs` | `Main.lean` |

`Fuzzer.hs` was removed. It had no module header (so it could not be imported
from the Haskell compiler either) and depended on `System.Random`; it was a
development tool rather than part of the compiler.

## Deliberate differences

These are forced by Lean or by the absence of Haskell's laziness/partiality.
They do not change the observable behaviour of well-formed programs.

- **Names.** `Type` is called `MTType` (Lean reserves `Type`), and `ST` is
  called `St` (Lean already has an `ST` monad).
- **State monads.** `State.lean` is a 1:1 port of Haskell's `ST`/`StateIO`.
  Lean's `StateM σ α` and `StateT σ IO α` are definitionally the same types
  (`σ → α × σ` and `σ → IO (α × σ)`); a later pass will move to them, using
  `get`/`set`/`modify` and `MonadState`/`MonadLift`. The custom module is kept
  for textual fidelity for now.
- **Strings.** The parser works on `Str := List Char` (Haskell's `String`),
  converting the source once in `parse`. Identifiers and TAM labels are Lean
  `String`, so `String.ofList` appears only when a token is captured; there is
  no per-character `String`/`List Char` round trip anymore. `string` and
  `caseless` still take Lean `String` literals and return the literal on
  success, as the Haskell code does.
- **Termination.** Non-structural definitions use `partial`: the parser
  (`many`, the mutually recursive expression/command parsers, `separatedBy`),
  the state-monad code generators, and the VM loop. They execute normally but
  cannot be unfolded in proofs yet.
- **Failure.** By default Lean's `panic!` reports a message and returns
  `default` instead of aborting, so `main` sets `LEAN_ABORT_ON_PANIC=1` (through
  `Std.Async.System`) before doing anything else. Every `panic!` then aborts like
  Haskell's `error` / `!!` / `head`. The panic text differs from GHC and the
  process dies by `SIGABRT` (status 134) rather than exit status 1, but both fail
  without writing an output file. This works in the compiled executable and in
  `lake env lean --run` (no user-defined extern is involved).
- **Integers.** The AST and VM use Lean's arbitrary-precision `Int`, while the
  Haskell VM uses 64-bit `Int` and truncates literals with `fromIntegral`.
  `DIV` uses `Int.fdiv` to match Haskell's floor division (`div`). Haskell's
  `div` aborts on a zero divisor, whereas `Int.fdiv` returns `0`; this is the
  one known behavioural difference at runtime and is left as-is.
- **Input.** Lean's `FS.Stream.getLine` keeps the line break, so the VM trims
  the line before parsing it with `String.toInt?`.

## Validation

The port was differentially tested against the Haskell binary (`ghc -O1`) on
the `cw2` test programs: generated TAM, compiler stdout/stderr (including type
errors), interpreter output, final stack, and `--trace` output are identical.
Older `cw1` sources are rejected by both compilers (they predate the typed
syntax).
