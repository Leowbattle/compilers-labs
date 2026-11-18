/-
  Direct port of `TAMParser.hs`.
-/

import Parser
import CodeGenerator

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
  arg1 "LOADL" TAMInst.LOADL natural <|>
  arg0 "ADD" TAMInst.ADD <|>
  arg0 "SUB" TAMInst.SUB <|>
  arg0 "MUL" TAMInst.MUL <|>
  arg0 "DIV" TAMInst.DIV <|>
  arg0 "NEG" TAMInst.NEG <|>
  arg0 "AND" TAMInst.AND <|>
  arg0 "OR" TAMInst.OR <|>
  arg0 "NOT" TAMInst.NOT <|>
  arg0 "LSS" TAMInst.LSS <|>
  arg0 "GRT" TAMInst.GRT <|>
  arg0 "EQL" TAMInst.EQL <|>
  arg0 "HALT" TAMInst.HALT <|>
  arg0 "GETINT" TAMInst.GETINT <|>
  arg0 "PUTINT" TAMInst.PUTINT <|>
  arg1 "LABEL" TAMInst.LABEL label <|>
  arg1 "JUMPIFZ" TAMInst.JUMPIFZ label <|>
  arg1 "JUMP" TAMInst.JUMP label <|>
  arg1 "CALL" TAMInst.CALL label <|>

  (do
    let _ ← token (caseless "RETURN")
    let x ← token natural
    let y ← token natural
    return TAMInst.RETURN x y) <|>

  (do
    let _ ← token (string "LOAD")
    let _ ← token (string "[")
    let modeS ← token (string "SB" <|> string "LB")
    let opS ← token (string "+" <|> string "-")
    let offset ← token natural
    let _ ← token (string "]")
    let mode := match modeS with
      | "SB" => StackMode.StackBase
      | "LB" => StackMode.LocalBase
      | _ => panic! "instTAM: invalid stack mode"
    let op := match opS with
      | "+" => AddressOperation.Plus
      | "-" => AddressOperation.Minus
      | _ => panic! "instTAM: invalid address operation"
    return TAMInst.LOAD mode op offset) <|>

  (do
    let _ ← token (string "STORE")
    let _ ← token (string "[")
    let modeS ← token (string "SB" <|> string "LB")
    let opS ← token (string "+" <|> string "-")
    let offset ← token natural
    let _ ← token (string "]")
    let mode := match modeS with
      | "SB" => StackMode.StackBase
      | "LB" => StackMode.LocalBase
      | _ => panic! "instTAM: invalid stack mode"
    let op := match opS with
      | "+" => AddressOperation.Plus
      | "-" => AddressOperation.Minus
      | _ => panic! "instTAM: invalid address operation"
    return TAMInst.STORE mode op offset)

def insts : Parser (List TAMInst) := many instTAM

def parseTAM (src : String) : List TAMInst :=
  match parse insts src with
  | (code, _) :: _ => code
  | [] => panic! "parseTAM: no parse"
