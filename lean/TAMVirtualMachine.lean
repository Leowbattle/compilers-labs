/-
  Direct port of `TAMVirtualMachine.hs`.
-/

import Parser
import CodeGenerator

open TAMInst StackMode AddressOperation

structure TAMState where
  tsCode : List TAMInst
  tsCounter : Int
  tsStack : Stack
  tsSB : Int
  tsLB : Int
  deriving Inhabited, Repr

abbrev TAMSt (α : Type) := StateT TAMState (ExceptT String IO) α

def liftIO {α : Type} (act : IO α) : TAMSt α :=
  StateT.lift (ExceptT.lift act)

def tsPush (x : Int) (ts : TAMState) : TAMState :=
  { ts with tsStack := x :: ts.tsStack }

def tsPop (ts : TAMState) : Except String (Int × TAMState) :=
  match ts.tsStack with
  | x :: xs => .ok (x, { ts with tsStack := xs })
  | [] => .error "stack underflow"

def tsSetCounter (i : Int) (ts : TAMState) : TAMState :=
  { ts with tsCounter := i }

def tsSetLB (lb : Int) (ts : TAMState) : TAMState :=
  { ts with tsLB := lb }

def tsSetLBT (lb : Int) : TAMSt Unit := do
  let state ← get
  set (tsSetLB lb state)

def listGet {α : Type} (l : List α) (i : Int) : Except String α :=
  if i < 0 then
    .error "negative index"
  else
    match l[i.toNat]? with
    | some x => .ok x
    | none => .error "index too large"

def tsInstruction (ts : TAMState) : Except String TAMInst :=
  listGet ts.tsCode ts.tsCounter

def counterT : TAMSt Int := do
  let ts ← get
  return ts.tsCounter

def instructionT : TAMSt TAMInst := do
  let st ← get
  let pc ← counterT
  liftExcept (listGet st.tsCode pc)

def countUpdateT (c : Int) : TAMSt Unit := do
  let ts ← get
  set { ts with tsCounter := c }

def continueT : TAMSt Unit := do
  let c ← counterT
  countUpdateT (c + 1)

def pushT (x : Int) : TAMSt Unit := do
  let ts ← get
  set (tsPush x ts)

def popT : TAMSt Int := do
  let ts ← get
  let (x, ts') ← liftExcept (tsPop ts)
  set ts'
  return x

def findLabelIdx : Nat → List TAMInst → Label → Option Nat
  | _, [], _ => none
  | i, x :: xs, l =>
      match x with
      | LABEL l' => if l == l' then some i else findLabelIdx (i + 1) xs l
      | _ => findLabelIdx (i + 1) xs l

def lookupLabel (prog : List TAMInst) (l : Label) : Except String Int :=
  match findLabelIdx 0 prog l with
  | some i => .ok (Int.ofNat i)
  | none => .error s!"label not found: {l}"

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

def jumpToLabel (l : Label) : TAMSt Unit := do
  let st ← get
  let i ← liftExcept (lookupLabel st.tsCode l)
  countUpdateT i

def varLookup (mode : StackMode) (op : AddressOperation) (a : Int) : TAMSt Int := do
  let st ← get
  let base := if mode == StackBase then st.tsSB else st.tsLB
  let offset := if op == Plus then a else -a
  return (Int.ofNat st.tsStack.length) - (base + offset) - 1

def getVar (mode : StackMode) (op : AddressOperation) (a : Int) : TAMSt Int := do
  let st ← get
  let addr ← varLookup mode op a
  match listGet st.tsStack addr with
  | .ok x => return x
  | .error e => throw e

def setVar (a x : Int) : TAMSt Unit := do
  if a < 0 then
    throw "negative stack address"
  let ts ← get
  let (as, rest) := ts.tsStack.splitAt a.toNat
  match rest with
  | _ :: bs => set { ts with tsStack := as ++ [x] ++ bs }
  | [] => throw "stack address out of bounds"

def replicateMT {α : Type} : Nat → TAMSt α → TAMSt (List α)
  | 0, _ => return []
  | n + 1, act => do
      let x ← act
      let xs ← replicateMT n act
      return (x :: xs)

def replicateMT_ {α : Type} (n : Nat) (act : TAMSt α) : TAMSt Unit :=
  match n with
  | 0 => return ()
  | n + 1 => do
      let _ ← act
      replicateMT_ n act

def readInt (s : String) : Except String Int :=
  match s.trimAscii.toString.toInt? with
  | some x => .ok x
  | none => .error "Prelude.read: no parse"

def executeT (inst : TAMInst) : TAMSt Unit :=
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
      if x == 0 then
        throw "division by zero"
      else
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
      liftIO (IO.println "Enter a number: ")
      let stdin ← liftIO IO.getStdin
      let x ← liftIO stdin.getLine
      match readInt x with
      | .ok n =>
          pushT n
          continueT
      | .error e => throw e

  | PUTINT => do
      liftIO (IO.print "output > ")
      let x ← popT
      liftIO (IO.println x)
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
      let st ← get
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

partial def execTAM : TAMSt Unit := do
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

def runTAM (prog : List TAMInst) : IO (Except String Stack) := do
  let r ← ExceptT.run (StateT.run execTAM (initialState prog))
  match r with
  | .ok (_, st) => return .ok st.tsStack
  | .error e => return .error e

partial def traceTAM' : TAMSt Unit := do
  let inst ← instructionT
  executeT inst
  let st ← get
  let pc ← counterT
  if pc == -1 then
    return ()
  else do
    match listGet st.tsCode pc with
    | .ok inst =>
        liftIO (IO.println (toString inst ++ "\t\t" ++ showIntList st.tsStack))
        traceTAM'
    | .error e => throw e

def traceTAM (prog : List TAMInst) : IO (Except String Stack) := do
  let r ← ExceptT.run (StateT.run traceTAM' (initialState prog))
  match r with
  | .ok (_, st) => return .ok st.tsStack
  | .error e => return .error e
