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

def fromJust {α : Type} [Inhabited α] (o : Option α) : α :=
  match o with
  | some x => x
  | none => panic! "fromJust Nothing"

def fromBool : Bool → Int
  | false => 0
  | true => 1

def varAddr (v : VarEnv) (x : String) : StackMode × Int :=
  fromJust (List.lookup x v)

def getLabel : StateM Int Label := do
  let n ← get
  set (n + 1)
  return "#" ++ toString n

mutual
  partial def binOpCode (inst : List TAMInst) (v : VarEnv) (x y : Expr) : StateM Int (List TAMInst) := do
    let xcode ← expCode v x
    let ycode ← expCode v y
    return (xcode ++ ycode ++ inst)

  partial def unOpCode (inst : List TAMInst) (v : VarEnv) (x : Expr) : StateM Int (List TAMInst) := do
    let xcode ← expCode v x
    return (xcode ++ inst)

  partial def expCode (v : VarEnv) (e : Expr) : StateM Int (List TAMInst) :=
    match e with
    | LitInteger x => return [LOADL x]
    | LitBool x => return [LOADL (fromBool x)]
    | Expr.Identifier x =>
        return (match varAddr v x with
          | (StackBase, offset) => [LOAD StackBase Plus offset]
          | (LocalBase, offset) => [LOAD LocalBase Minus offset])

    | BinOp Addition x y => binOpCode [ADD] v x y
    | BinOp Subtraction x y => binOpCode [SUB] v x y
    | BinOp Multiplication x y => binOpCode [MUL] v x y
    | BinOp Division x y => binOpCode [DIV] v x y

    | BinOp BinAnd x y => binOpCode [AND] v x y
    | BinOp BinOr x y => binOpCode [OR] v x y

    | BinOp Less x y => binOpCode [LSS] v x y
    | BinOp LessEq x y => binOpCode [LOADL 1, ADD, LSS] v x y

    | BinOp Greater x y => binOpCode [GRT] v x y
    | BinOp GreaterEq x y => binOpCode [LOADL 1, SUB, GRT] v x y

    | BinOp Equals x y => binOpCode [EQL] v x y
    | BinOp NotEquals x y => binOpCode [EQL, NOT] v x y

    | UnOp Negation x => unOpCode [NEG] v x
    | UnOp UnNot x => unOpCode [NOT] v x

    | Conditional cond yes no => do
        let condcode ← expCode v cond
        let yescode ← expCode v yes
        let l1 ← getLabel
        let nocode ← expCode v no
        let l2 ← getLabel
        return (condcode ++
                [JUMPIFZ l1] ++
                yescode ++
                [JUMP l2, LABEL l1] ++
                nocode ++
                [LABEL l2])

    | FunctionCall id args => do
        let argscode ← List.flatten <$> args.mapM (expCode v)
        return (argscode ++ [CALL id])
end

partial def compCmd (v : VarEnv) (cmd : Command) : StateM Int (List TAMInst) :=
  match cmd with
  | Assignment x expr => do
      let ecode ← expCode v expr
      let store := match varAddr v x with
        | (StackBase, offset) => [STORE StackBase Plus offset]
        | (LocalBase, offset) => [STORE LocalBase Minus offset]
      return (ecode ++ store)

  | IfThenElse expr yes no => do
      let ecode ← expCode v expr
      let yescode ← compCmd v yes
      let l1 ← getLabel
      let nocode ← compCmd v no
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
      let cmdCode ← compCmd v cmd
      let l2 ← getLabel
      return ([LABEL l1] ++
              ecode ++
              [JUMPIFZ l2] ++
              cmdCode ++
              [JUMP l1, LABEL l2])

  | GetInt x =>
      return (match varAddr v x with
        | (StackBase, offset) => [GETINT, STORE StackBase Plus offset]
        | (LocalBase, offset) => [GETINT, STORE LocalBase Minus offset])

  | PrintInt expr => do
      let ecode ← expCode v expr
      return (ecode ++ [PUTINT])

  | BeginEnd [] => return []
  | BeginEnd (c :: cs) => do
      let ccode ← compCmd v c
      let cscode ← compCmd v (BeginEnd cs)
      return (ccode ++ cscode)

partial def compileVars (inst : List TAMInst) (ds : List Declaration) (v : VarEnv) : StateM Int (VarEnv × List TAMInst) :=
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

def compileFunction (v : VarEnv) : Declaration → StateM Int (List TAMInst)
  | FunctionDecl id args _ret e => do
      let v' := args.reverse.zipWith (fun (arg, _) i => (arg, (LocalBase, i)))
                                      (List.range' 1 args.length) ++ v
      let ecode ← expCode v' e
      return ([LABEL id] ++ ecode ++ [RETURN 1 (Int.ofNat args.length)])
  | _ => return []

def compileProgram : Program → StateM Int (List TAMInst)
  | LetIn decls cmd => do
      let (v, initCode) ← compileVars [] decls []
      let cmdCode ← compCmd v cmd
      let funcCode ← List.flatten <$> decls.mapM (compileFunction v)
      return (initCode ++ cmdCode ++ [HALT] ++ funcCode)

def compileMT (s : String) : List TAMInst × Error :=
  let ast := parseMT s
  let err := (StateT.run (checkProgram ast) []).1
  let inst := if err == "" then (StateT.run (compileProgram ast) 0).1 else []
  (inst, err)
