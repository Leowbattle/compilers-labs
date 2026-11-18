/-
  Direct port of `Main.hs`.
-/

import Std.Async.System

import Parser
import MTParser
import TAMParser
import TAMVirtualMachine
import CodeGenerator
import TypeChecker

/--
Make `panic!` abort the process (like Haskell's `error`). Without this, compiled
Lean reports the panic and returns `default`. The runtime reads this environment
variable when a panic occurs, so setting it here works in compiled and
interpreted code.
-/
def enableAbortOnPanic : IO Unit :=
  Std.Async.System.setEnvVar "LEAN_ABORT_ON_PANIC" "1"

def takeExtension (s : String) : String :=
  let cs : Str := s.toList.reverse
  String.ofList ((cs.takeWhile fun c => c != '.').reverse)

def removeExtension (s : String) : String :=
  let cs : Str := s.toList.reverse
  match cs.dropWhile fun c => c != '.' with
  | _ :: tl => String.ofList tl.reverse
  | [] => panic! "removeExtension: no extension"

def runCompiler (srcFile : String) : IO Unit := do
  let src ← IO.FS.readFile srcFile
  let (code, error) := compileMT src
  let outPath := removeExtension srcFile ++ ".tam"
  IO.print (if error == "" then "" else error ++ "\n")
  IO.println s!"compiled to TAM file: {outPath}"
  IO.FS.writeFile outPath (showTAM code)

def runInterpreter (srcFile : String) (trace : Bool) : IO Unit := do
  let src ← IO.FS.readFile srcFile
  let code := parseTAM src
  let result ← if trace then
    traceTAM code
  else
    runTAM code
  IO.print "Final stack: "
  IO.println (showIntList result)

def main (args : List String) : IO Unit := do
  enableAbortOnPanic
  match args with
  | [] => panic! "Prelude.head: empty list"
  | srcFile :: _ =>
      let trace := args.contains "--trace"
      match takeExtension srcFile with
      | "mt" => runCompiler srcFile
      | "tam" => runInterpreter srcFile trace
      | _ => panic! "main: unsupported file extension"
