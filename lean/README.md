# MT compiler in Lean 4

A direct port of the Haskell `cw2` compiler that lives in the repository root.
The goal is to reproduce the Haskell behaviour module for module. The parser,
type checker, and code generator are total; the VM run loop is intentionally
`partial`, because a TAM program may not terminate.

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
| `MTParser.hs` | `MTParser.lean` |
| `TAMParser.hs` | `TAMParser.lean` |
| `TypeChecker.hs` | `TypeChecker.lean` |
| `CodeGenerator.hs` | `CodeGenerator.lean` |
| `TAMVirtualMachine.hs` | `TAMVirtualMachine.lean` |
| `Main.hs` | `Main.lean` |

`State.hs` is not ported as a module; the Lean code uses `StateM`/`StateT` directly.

`Fuzzer.hs` was removed. It had no module header (so it could not be imported
from the Haskell compiler either) and depended on `System.Random`; it was a
development tool rather than part of the compiler.

## Deliberate differences

These are forced by Lean or by the absence of Haskell's laziness/partiality.
They do not change the observable behaviour of well-formed programs.

- **Names.** `Type` is called `MTType` (Lean reserves `Type`).
- **State monads.** `State.hs` is not ported as a module. The Lean code uses
  Lean's `StateM σ α` / `StateT σ IO α` directly, with `get`/`set`/`modify`,
  `StateT.run`, and `StateT.lift`. The VM wraps the state monad in
  `ExceptT String IO` so runtime failures are values, not panics.
- **Strings.** The parser works on `Str := List Char` (Haskell's `String`),
  converting the source once in `parse`. Identifiers and TAM labels are Lean
  `String`, so `String.ofList` appears only when a token is captured; there is
  no per-character `String`/`List Char` round trip anymore. `string` and
  `caseless` still take Lean `String` literals and return the literal on
  success, as the Haskell code does.
- **Termination.** `many` and the mutually recursive expression/command
  parsers are total. They recurse only on strictly shorter input; there is no
  fuel parameter. The parser uses the total `parserFix` combinator: a recursive
  call on the same or longer input returns no results instead of looping.
  The type checker and code generator use the same structural measure approach.
  The only remaining `partial` definitions are the VM run loop (`execTAM`) and
  its tracing variant (`traceTAM'`), because TAM execution may not terminate.
- **Errors.** There are no `panic!` calls. Parsing, type checking, and code
  generation return `Except String`; the VM uses
  `StateT TAMState (ExceptT String IO)`. The CLI prints the error and exits
  nonzero. In particular, division by zero and invalid TAM/input are reported
  as runtime errors.
- **Integers.** The AST and VM use Lean's arbitrary-precision `Int`, while the
  Haskell VM uses 64-bit `Int` and truncates literals with `fromIntegral`.
  `DIV` uses `Int.fdiv` to match Haskell's floor division (`div`). A zero
  divisor is reported as a runtime error instead of being silently mapped to
  `0`.
- **Input.** Lean's `FS.Stream.getLine` keeps the line break, so the VM trims
  the line before parsing it with `String.toInt?`.

## Validation

The port was differentially tested against the Haskell binary (`ghc -O1`) on
the `cw2` test programs and the generated benchmark corpus: generated TAM,
compiler stdout/stderr (including type errors), interpreter output, final
stack, and `--trace` output are identical. Older `cw1` sources are rejected by
both compilers (they predate the typed syntax).

For the totalization check:

```sh
grep -R "panic!" lean/*.lean
grep -R "partial def" lean/*.lean
# only execTAM and traceTAM' should remain partial
```
