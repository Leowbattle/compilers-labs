/-
  Direct port of `Main.hs`.
-/

import Parser
import MTParser
import TAMParser
import TAMVirtualMachine
import CodeGenerator
import TypeChecker

def takeExtension (s : String) : String :=
  let cs : Str := s.toList.reverse
  String.ofList ((cs.takeWhile fun c => c != '.').reverse)

def removeExtension (s : String) : Option String :=
  let cs : Str := s.toList.reverse
  match cs.dropWhile fun c => c != '.' with
  | _ :: tl => some (String.ofList tl.reverse)
  | [] => none

def runCompiler (srcFile : String) : IO Unit := do
  let src ← IO.FS.readFile srcFile
  match compileMT src with
  | .error e =>
      IO.eprintln s!"error: {e}"
      IO.Process.exit 1
  | .ok (code, error) =>
      match removeExtension srcFile with
      | none =>
          IO.eprintln "error: source file has no extension"
          IO.Process.exit 1
      | some base =>
          let outPath := base ++ ".tam"
          IO.print (if error == "" then "" else error ++ "\n")
          IO.println s!"compiled to TAM file: {outPath}"
          IO.FS.writeFile outPath (showTAM code)

def runInterpreter (srcFile : String) (trace : Bool) : IO Unit := do
  let src ← IO.FS.readFile srcFile
  match parseTAM src with
  | .error e =>
      IO.eprintln s!"parse error: {e}"
      IO.Process.exit 1
  | .ok code =>
      let result ← if trace then traceTAM code else runTAM code
      match result with
      | .ok stack =>
          IO.print "Final stack: "
          IO.println (showIntList stack)
      | .error e =>
          IO.eprintln s!"runtime error: {e}"
          IO.Process.exit 1

def main (args : List String) : IO Unit := do
  match args with
  | [] =>
      IO.eprintln "usage: mtc <file.mt|file.tam> [--trace]"
      IO.Process.exit 1
  | srcFile :: _ =>
      let trace := args.contains "--trace"
      match takeExtension srcFile with
      | "mt" => runCompiler srcFile
      | "tam" => runInterpreter srcFile trace
      | ext =>
          IO.eprintln s!"error: unsupported file extension: {ext}"
          IO.Process.exit 1
