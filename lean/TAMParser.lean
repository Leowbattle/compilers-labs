/-
  Direct port of `TAMParser.hs`.
-/

import Parser
import CodeGenerator

open TAMInst StackMode AddressOperation

def arg0 (s : String) (i : TAMInst) : Parser TAMInst := token (do
  let _ ← caseless s
  return i)

def arg1 {α : Type} (s : String) (c : α → TAMInst) (p : Parser α) : Parser TAMInst := do
  let _ ← token (caseless s)
  let x ← token p
  return (c x)

def label : Parser String := do
  let cs : Str ← many (satisfy item fun c => !c.isWhitespace)
  return String.ofList cs

def instTAM : Parser TAMInst :=
  arg1 "LOADL" LOADL natural <|>
  arg0 "ADD" ADD <|>
  arg0 "SUB" SUB <|>
  arg0 "MUL" MUL <|>
  arg0 "DIV" DIV <|>
  arg0 "NEG" NEG <|>
  arg0 "AND" AND <|>
  arg0 "OR" OR <|>
  arg0 "NOT" NOT <|>
  arg0 "LSS" LSS <|>
  arg0 "GRT" GRT <|>
  arg0 "EQL" EQL <|>
  arg0 "HALT" HALT <|>
  arg0 "GETINT" GETINT <|>
  arg0 "PUTINT" PUTINT <|>
  arg1 "LABEL" LABEL label <|>
  arg1 "JUMPIFZ" JUMPIFZ label <|>
  arg1 "JUMP" JUMP label <|>
  arg1 "CALL" CALL label <|>

  (do
    let _ ← token (caseless "RETURN")
    let x ← token natural
    let y ← token natural
    return RETURN x y) <|>

  (do
    let _ ← token (string "LOAD")
    let _ ← token (string "[")
    let modeS ← token (string "SB" <|> string "LB")
    let opS ← token (string "+" <|> string "-")
    let offset ← token natural
    let _ ← token (string "]")
    let mode := match modeS with
      | "SB" => StackBase
      | "LB" => LocalBase
      | _ => panic! "instTAM: invalid stack mode"
    let op := match opS with
      | "+" => Plus
      | "-" => Minus
      | _ => panic! "instTAM: invalid address operation"
    return LOAD mode op offset) <|>

  (do
    let _ ← token (string "STORE")
    let _ ← token (string "[")
    let modeS ← token (string "SB" <|> string "LB")
    let opS ← token (string "+" <|> string "-")
    let offset ← token natural
    let _ ← token (string "]")
    let mode := match modeS with
      | "SB" => StackBase
      | "LB" => LocalBase
      | _ => panic! "instTAM: invalid stack mode"
    let op := match opS with
      | "+" => Plus
      | "-" => Minus
      | _ => panic! "instTAM: invalid address operation"
    return STORE mode op offset)

def insts : Parser (List TAMInst) := many instTAM

def parseTAM (src : String) : List TAMInst :=
  match parse insts src with
  | (code, _) :: _ => code
  | [] => panic! "parseTAM: no parse"
