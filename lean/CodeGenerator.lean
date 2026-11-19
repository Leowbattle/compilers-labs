/-
  Direct port of `CodeGenerator.hs`.
-/

import Parser
import MTParser
import TypeChecker

open Command Declaration Program BinOperator UnOperator
-- Excludes `Identifier`, which would otherwise shadow the `Identifier` type alias.
open Expr (LitInteger LitBool BinOp UnOp Conditional FunctionCall)

abbrev Stack := List Int

abbrev Label := String

inductive StackMode
  | StackBase
  | LocalBase
  deriving BEq, Inhabited

open StackMode

instance : ToString StackMode where
  toString
    | StackBase => "SB"
    | LocalBase => "LB"

instance : Repr StackMode where
  reprPrec
    | StackBase, _ => "SB"
    | LocalBase, _ => "LB"

inductive AddressOperation
  | Plus
  | Minus
  deriving BEq, Inhabited

open AddressOperation

instance : ToString AddressOperation where
  toString
    | Plus => "+"
    | Minus => "-"

instance : Repr AddressOperation where
  reprPrec
    | Plus, _ => "+"
    | Minus, _ => "-"

inductive TAMInst
  | LOADL (x : Int)
  | ADD
  | SUB
  | MUL
  | DIV
  | NEG
  | AND
  | OR
  | NOT
  | LSS
  | GRT
  | EQL
  | HALT
  | GETINT
  | PUTINT
  | LABEL (l : Label)
  | JUMP (l : Label)
  | JUMPIFZ (l : Label)
  | CALL (l : Label)
  | RETURN (x y : Int)
  | LOAD (mode : StackMode) (op : AddressOperation) (a : Int)
  | STORE (mode : StackMode) (op : AddressOperation) (a : Int)
  deriving BEq, Repr, Inhabited

open TAMInst

instance : ToString TAMInst where
  toString
    | LOADL x => "LOADL " ++ toString x
    | ADD => "ADD"
    | SUB => "SUB"
    | MUL => "MUL"
    | DIV => "DIV"
    | NEG => "NEG"
    | AND => "AND"
    | OR => "OR"
    | NOT => "NOT"
    | LSS => "LSS"
    | GRT => "GRT"
    | EQL => "EQL"
    | HALT => "HALT"
    | GETINT => "GETINT"
    | PUTINT => "PUTINT"
    | LABEL l => "LABEL " ++ l
    | JUMP l => "JUMP " ++ l
    | JUMPIFZ l => "JUMPIFZ " ++ l
    | CALL l => "CALL " ++ l
    | RETURN x y => "RETURN " ++ toString x ++ " " ++ toString y
    | LOAD mode op a =>
        "LOAD [" ++ toString mode ++ " " ++ toString op ++ " " ++ toString a ++ "]"
    | STORE mode op a =>
        "STORE [" ++ toString mode ++ " " ++ toString op ++ " " ++ toString a ++ "]"

def showTAM (prog : List TAMInst) : String :=
  String.intercalate "\n" (prog.map toString)

abbrev VarEnv := List (String × (StackMode × Int))

abbrev Gen (α : Type) := StateT Int (Except String) α

def fromBool : Bool → Int
  | false => 0
  | true => 1

def varAddr (v : VarEnv) (x : String) : Option (StackMode × Int) :=
  List.lookup x v

def getLabel : Gen Label := do
  let n ← get
  set (n + 1)
  return "#" ++ toString n

def cmdSize : Command → Nat
  | Assignment _ e => 1 + exprSize e
  | IfThenElse e a b => 1 + exprSize e + cmdSize a + cmdSize b
  | While e c => 1 + exprSize e + cmdSize c
  | GetInt _ => 1
  | PrintInt e => 1 + exprSize e
  | BeginEnd cs => 1 + (cs.map cmdSize).foldl (· + ·) 0

def varLoad (v : VarEnv) (x : String) : Gen (List TAMInst) :=
  match varAddr v x with
  | some (StackBase, offset) => return [LOAD StackBase Plus offset]
  | some (LocalBase, offset) => return [LOAD LocalBase Minus offset]
  | none => throw s!"unbound variable: {x}"

def varStore (v : VarEnv) (x : String) : Gen (List TAMInst) :=
  match varAddr v x with
  | some (StackBase, offset) => return [STORE StackBase Plus offset]
  | some (LocalBase, offset) => return [STORE LocalBase Minus offset]
  | none => throw s!"unbound variable: {x}"

def varGetInt (v : VarEnv) (x : String) : Gen (List TAMInst) :=
  match varAddr v x with
  | some (StackBase, offset) => return [GETINT, STORE StackBase Plus offset]
  | some (LocalBase, offset) => return [GETINT, STORE LocalBase Minus offset]
  | none => throw s!"unbound variable: {x}"

def binOpCode (rec : VarEnv → Expr → Gen (List TAMInst))
    (inst : List TAMInst) (v : VarEnv) (x y : Expr) : Gen (List TAMInst) := do
  let xcode ← rec v x
  let ycode ← rec v y
  return (xcode ++ ycode ++ inst)

def unOpCode (rec : VarEnv → Expr → Gen (List TAMInst))
    (inst : List TAMInst) (v : VarEnv) (x : Expr) : Gen (List TAMInst) := do
  let xcode ← rec v x
  return (xcode ++ inst)

def expCodeBody (v : VarEnv) (rec : VarEnv → Expr → Gen (List TAMInst)) :
    Expr → Gen (List TAMInst)
  | LitInteger x => return [LOADL x]
  | LitBool x => return [LOADL (fromBool x)]
  | Expr.Identifier x => varLoad v x

  | BinOp Addition x y => binOpCode rec [ADD] v x y
  | BinOp Subtraction x y => binOpCode rec [SUB] v x y
  | BinOp Multiplication x y => binOpCode rec [MUL] v x y
  | BinOp Division x y => binOpCode rec [DIV] v x y

  | BinOp BinAnd x y => binOpCode rec [AND] v x y
  | BinOp BinOr x y => binOpCode rec [OR] v x y

  | BinOp Less x y => binOpCode rec [LSS] v x y
  | BinOp LessEq x y => binOpCode rec [LOADL 1, ADD, LSS] v x y

  | BinOp Greater x y => binOpCode rec [GRT] v x y
  | BinOp GreaterEq x y => binOpCode rec [LOADL 1, SUB, GRT] v x y

  | BinOp Equals x y => binOpCode rec [EQL] v x y
  | BinOp NotEquals x y => binOpCode rec [EQL, NOT] v x y

  | UnOp Negation x => unOpCode rec [NEG] v x
  | UnOp UnNot x => unOpCode rec [NOT] v x

  | Conditional cond yes no => do
      let condcode ← rec v cond
      let yescode ← rec v yes
      let l1 ← getLabel
      let nocode ← rec v no
      let l2 ← getLabel
      return (condcode ++
              [JUMPIFZ l1] ++
              yescode ++
              [JUMP l2, LABEL l1] ++
              nocode ++
              [LABEL l2])

  | FunctionCall id args => do
      let argscode ← List.flatten <$> args.mapM (rec v)
      return (argscode ++ [CALL id])

def expCode (v : VarEnv) : Expr → Gen (List TAMInst)
  | e =>
      expCodeBody v
        (fun v' x => if _h : exprSize x < exprSize e then expCode v' x
                     else throw "codegen: recursive expression did not decrease")
        e
termination_by e => exprSize e

def compCmdBody (v : VarEnv)
    (rec : VarEnv → Command → Gen (List TAMInst)) : Command → Gen (List TAMInst)
  | Assignment x expr => do
      let ecode ← expCode v expr
      let store ← varStore v x
      return (ecode ++ store)

  | IfThenElse expr yes no => do
      let ecode ← expCode v expr
      let yescode ← rec v yes
      let l1 ← getLabel
      let nocode ← rec v no
      let l2 ← getLabel
      return (ecode ++
              [JUMPIFZ l1] ++
              yescode ++
              [JUMP l2, LABEL l1] ++
              nocode ++
              [LABEL l2])

  | While expr cmd => do
      let ecode ← expCode v expr
      let l1 ← getLabel
      let cmdCode ← rec v cmd
      let l2 ← getLabel
      return ([LABEL l1] ++
              ecode ++
              [JUMPIFZ l2] ++
              cmdCode ++
              [JUMP l1, LABEL l2])

  | GetInt x => varGetInt v x

  | PrintInt expr => do
      let ecode ← expCode v expr
      return (ecode ++ [PUTINT])

  | BeginEnd [] => return []
  | BeginEnd (c :: cs) => do
      let ccode ← rec v c
      let cscode ← rec v (BeginEnd cs)
      return (ccode ++ cscode)

def compCmd (v : VarEnv) : Command → Gen (List TAMInst)
  | cmd =>
      compCmdBody v
        (fun v' sub => if _h : cmdSize sub < cmdSize cmd then compCmd v' sub
                       else throw "codegen: recursive command did not decrease")
        cmd
termination_by cmd => cmdSize cmd

def compileVars (inst : List TAMInst) (ds : List Declaration) (v : VarEnv) :
    Gen (VarEnv × List TAMInst) :=
  match ds with
  | [] => return (v, inst)
  | VarDecl id _ty :: rest =>
      let v' := (id, (StackBase, (Int.ofNat v.length))) :: v
      compileVars (inst ++ [LOADL 0]) rest v'
  | VarInit id _ty e :: rest => do
      let v' := (id, (StackBase, (Int.ofNat v.length))) :: v
      let ecode ← expCode v e
      compileVars (inst ++ ecode) rest v'
  | FunctionDecl _ _ _ _ :: rest => compileVars inst rest v

def compileFunction (v : VarEnv) : Declaration → Gen (List TAMInst)
  | FunctionDecl id args _ret e => do
      let v' := args.reverse.zipWith (fun (arg, _) i => (arg, (LocalBase, i)))
                                      (List.range' 1 args.length) ++ v
      let ecode ← expCode v' e
      return ([LABEL id] ++ ecode ++ [RETURN 1 (Int.ofNat args.length)])
  | _ => return []

def compileProgram : Program → Gen (List TAMInst)
  | LetIn decls cmd => do
      let (v, initCode) ← compileVars [] decls []
      let cmdCode ← compCmd v cmd
      let funcCode ← List.flatten <$> decls.mapM (compileFunction v)
      return (initCode ++ cmdCode ++ [HALT] ++ funcCode)

def compileMT (s : String) : Except String (List TAMInst × Error) := do
  let ast ← parseMT s
  let err := (StateT.run (checkProgram ast) []).1
  if err == "" then
    let (inst, _) ← StateT.run (compileProgram ast) 0
    return (inst, err)
  else
    return ([], err)
