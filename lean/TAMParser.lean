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

def stackModeP : Parser StackMode :=
  (string "SB" *> pure StackBase) <|> (string "LB" *> pure LocalBase)

def addressOperationP : Parser AddressOperation :=
  (string "+" *> pure Plus) <|> (string "-" *> pure Minus)

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
    let mode ← token stackModeP
    let op ← token addressOperationP
    let offset ← token natural
    let _ ← token (string "]")
    return LOAD mode op offset) <|>

  (do
    let _ ← token (string "STORE")
    let _ ← token (string "[")
    let mode ← token stackModeP
    let op ← token addressOperationP
    let offset ← token natural
    let _ ← token (string "]")
    return STORE mode op offset)

def insts : Parser (List TAMInst) := many instTAM

def parseTAM (src : String) : Except String (List TAMInst) :=
  match parseExact insts src with
  | code :: _ => .ok code
  | [] => .error "parse error"

/-- Successful `parseTAM` results consume the entire input. -/
theorem parseTAM_consumes (src : String) (code : List TAMInst)
    (h : parseTAM src = .ok code) :
    (code, []) ∈ parse insts src := by
  unfold parseTAM at h
  split at h
  · rename_i t ts hx
    have hp : t ∈ parseExact insts src := by
      rw [hx]
      exact List.mem_cons_self
    cases h
    exact mem_parseExact hp
  · simp at h
