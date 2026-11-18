# MT compiler (Haskell)

Build:

```sh
cabal build all
```

Run:

```sh
cabal run mtc -- path/to/program.mt          # compile to path/to/program.tam
cabal run mtc -- path/to/program.tam         # run the TAM interpreter
cabal run mtc -- path/to/program.tam --trace # run with a trace
```
