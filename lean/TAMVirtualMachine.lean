/-
  Direct port of `TAMVirtualMachine.hs`.
-/

import Parser
import State
import CodeGenerator

open TAMInst StackMode AddressOperation

structure TAMState where
  tsCode : List TAMInst
  tsCounter : Int
  tsStack : Stack
  tsSB : Int
  tsLB : Int
  deriving Inhabited, Repr

abbrev TAMSt (α : Type) := StateIO TAMState α

def tsPush (x : Int) (ts : TAMState) : TAMState :=
  { ts with tsStack := x :: ts.tsStack }

def tsPop (ts : TAMState) : Int × TAMState :=
  match ts.tsStack with
  | x :: xs => (x, { ts with tsStack := xs })
  | [] => panic! "tsPop: empty stack"

def tsSetCounter (i : Int) (ts : TAMState) : TAMState :=
  { ts with tsCounter := i }

def tsSetLB (lb : Int) (ts : TAMState) : TAMState :=
  { ts with tsLB := lb }

def tsSetLBT (lb : Int) : TAMSt Unit := do
  let state ← stStateT
  stUpdateT (tsSetLB lb state)

def listGetInt {α : Type} [Inhabited α] (l : List α) (i : Int) : α :=
  if i < 0 then
    panic! "negative index"
  else
    match l[i.toNat]? with
    | some x => x
    | none => panic! "index too large"

def tsInstruction (ts : TAMState) : TAMInst :=
  listGetInt ts.tsCode ts.tsCounter

def counterT : TAMSt Int := do
  let ts ← stStateT
  return ts.tsCounter

def instructionT : TAMSt TAMInst := do
  let st ← stStateT
  let pc ← counterT
  return (listGetInt st.tsCode pc)

def countUpdateT (c : Int) : TAMSt Unit := do
  let ts ← stStateT
  stUpdateT { ts with tsCounter := c }

def continueT : TAMSt Unit := do
  let c ← counterT
  countUpdateT (c + 1)

def pushT (x : Int) : TAMSt Unit := do
  let ts ← stStateT
  stUpdateT (tsPush x ts)

def popT : TAMSt Int := do
  let ts ← stStateT
  let (x, ts') := tsPop ts
  stUpdateT ts'
  return x

def findLabelIdx : Nat → List TAMInst → Label → Option Nat
  | _, [], _ => none
  | i, x :: xs, l =>
      match x with
      | LABEL l' => if l == l' then some i else findLabelIdx (i + 1) xs l
      | _ => findLabelIdx (i + 1) xs l

def lookupLabel (prog : List TAMInst) (l : Label) : Int :=
  match findLabelIdx 0 prog l with
  | some i => Int.ofNat i
  | none => panic! "lookupLabel: label not found"

def boolAnd (x y : Int) : Int :=
  match (x, y) with
  | (_, 0) => 0
  | (0, _) => 0
  | (_, _) => 1

def boolOr (x y : Int) : Int :=
  match (x, y) with
  | (0, 0) => 0
  | (_, _) => 1

def boolNot (x : Int) : Int :=
  match x with
  | 0 => 1
  | _ => 0

def jumpToLabel (l : Label) : StateIO TAMState Unit := do
  let st ← stStateT
  countUpdateT (lookupLabel st.tsCode l)

def varLookup (mode : StackMode) (op : AddressOperation) (a : Int) : StateIO TAMState Int := do
  let st ← stStateT
  let base := if mode == StackBase then st.tsSB else st.tsLB
  let offset := if op == Plus then a else -a
  return (Int.ofNat st.tsStack.length) - (base + offset) - 1

def getVar (mode : StackMode) (op : AddressOperation) (a : Int) : StateIO TAMState Int := do
  let st ← stStateT
  let addr ← varLookup mode op a
  return (listGetInt st.tsStack addr)

def setVar (a x : Int) : StateIO TAMState Unit := do
  let ts ← stStateT
  let (as, rest) := ts.tsStack.splitAt a.toNat
  match rest with
  | _ :: bs => stUpdateT { ts with tsStack := as ++ [x] ++ bs }
  | [] => panic! "setVar: index out of bounds"

partial def replicateMT {α : Type} : Nat → TAMSt α → TAMSt (List α)
  | 0, _ => return []
  | n + 1, act => do
      let x ← act
      let xs ← replicateMT n act
      return (x :: xs)

partial def replicateMT_ {α : Type} (n : Nat) (act : TAMSt α) : TAMSt Unit :=
  match n with
  | 0 => return ()
  | n + 1 => do
      let _ ← act
      replicateMT_ n act

def readInt (s : String) : Int :=
  match s.trimAscii.toString.toInt? with
  | some x => x
  | none => panic! "Prelude.read: no parse"

def executeT (inst : TAMInst) : StateIO TAMState Unit :=
  match inst with
  | LOADL x => do
      pushT x
      continueT

  | ADD => do
      let x ← popT
      let y ← popT
      pushT (x + y)
      continueT

  | SUB => do
      let x ← popT
      let y ← popT
      pushT (y - x)
      continueT

  | MUL => do
      let x ← popT
      let y ← popT
      pushT (x * y)
      continueT

  | DIV => do
      let x ← popT
      let y ← popT
      pushT (Int.fdiv y x)
      continueT

  | LSS => do
      let y ← popT
      let x ← popT
      pushT (if x < y then 1 else 0)
      continueT

  | GRT => do
      let y ← popT
      let x ← popT
      pushT (if x > y then 1 else 0)
      continueT

  | EQL => do
      let x ← popT
      let y ← popT
      pushT (if x == y then 1 else 0)
      continueT

  | AND => do
      let x ← popT
      let y ← popT
      pushT (boolAnd x y)
      continueT

  | OR => do
      let x ← popT
      let y ← popT
      pushT (boolOr x y)
      continueT

  | NOT => do
      let x ← popT
      pushT (boolNot x)
      continueT

  | NEG => do
      let x ← popT
      pushT (-x)
      continueT

  | HALT => countUpdateT (-1)

  | GETINT => do
      lift (IO.println "Enter a number: ")
      let stdin ← lift IO.getStdin
      let x ← lift stdin.getLine
      pushT (readInt x)
      continueT

  | PUTINT => do
      lift (IO.print "output > ")
      let x ← popT
      lift (IO.println x)
      continueT

  | LABEL _ => continueT

  | JUMP l => jumpToLabel l

  | JUMPIFZ l => do
      let x ← popT
      if x == 0 then
        jumpToLabel l
      else
        continueT

  | LOAD mode op a => do
      let x ← getVar mode op a
      pushT x
      continueT

  | STORE mode op a => do
      let x ← popT
      let addr ← varLookup mode op a
      setVar addr x
      continueT

  | CALL l => do
      let st ← stStateT
      pushT st.tsLB
      pushT (st.tsCounter + 1)
      tsSetLBT (Int.ofNat st.tsStack.length)
      jumpToLabel l

  | RETURN n m => do
      let retVal ← replicateMT n.toNat popT
      let ra ← popT
      let lb ← popT
      replicateMT_ m.toNat popT
      retVal.forM pushT
      tsSetLBT lb
      countUpdateT ra

partial def execTAM : StateIO TAMState Unit := do
  let inst ← instructionT
  executeT inst
  let pc ← counterT
  if pc == -1 then
    return ()
  else
    execTAM

def initialState (prog : List TAMInst) : TAMState :=
  { tsCode := prog, tsCounter := 0, tsStack := [], tsSB := 0, tsLB := 0 }

def showIntList (xs : List Int) : String :=
  "[" ++ String.intercalate "," (xs.map toString) ++ "]"

def runTAM (prog : List TAMInst) : IO Stack := do
  let (_, st) ← appT execTAM (initialState prog)
  return st.tsStack

partial def traceTAM' : StateIO TAMState Unit := do
  let inst ← instructionT
  executeT inst
  let st ← stStateT
  let pc ← counterT
  if pc == -1 then
    return ()
  else do
    lift (IO.println (toString (listGetInt st.tsCode pc) ++ "\t\t" ++ showIntList st.tsStack))
    traceTAM'

def traceTAM (prog : List TAMInst) : IO Stack := do
  let (_, st) ← appT traceTAM' (initialState prog)
  return st.tsStack
