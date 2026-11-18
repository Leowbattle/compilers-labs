/-
  Direct port of `CodeGenerator.hs`.
-/

import Parser
import MTParser
import State
import TypeChecker

abbrev Stack := List Int

abbrev Label := String

inductive StackMode
  | StackBase
  | LocalBase
  deriving BEq, Inhabited

instance : ToString StackMode where
  toString
    | StackMode.StackBase => "SB"
    | StackMode.LocalBase => "LB"

instance : Repr StackMode where
  reprPrec
    | StackMode.StackBase, _ => "SB"
    | StackMode.LocalBase, _ => "LB"

inductive AddressOperation
  | Plus
  | Minus
  deriving BEq, Inhabited

instance : ToString AddressOperation where
  toString
    | AddressOperation.Plus => "+"
    | AddressOperation.Minus => "-"

instance : Repr AddressOperation where
  reprPrec
    | AddressOperation.Plus, _ => "+"
    | AddressOperation.Minus, _ => "-"

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

instance : ToString TAMInst where
  toString
    | TAMInst.LOADL x => "LOADL " ++ toString x
    | TAMInst.ADD => "ADD"
    | TAMInst.SUB => "SUB"
    | TAMInst.MUL => "MUL"
    | TAMInst.DIV => "DIV"
    | TAMInst.NEG => "NEG"
    | TAMInst.AND => "AND"
    | TAMInst.OR => "OR"
    | TAMInst.NOT => "NOT"
    | TAMInst.LSS => "LSS"
    | TAMInst.GRT => "GRT"
    | TAMInst.EQL => "EQL"
    | TAMInst.HALT => "HALT"
    | TAMInst.GETINT => "GETINT"
    | TAMInst.PUTINT => "PUTINT"
    | TAMInst.LABEL l => "LABEL " ++ l
    | TAMInst.JUMP l => "JUMP " ++ l
    | TAMInst.JUMPIFZ l => "JUMPIFZ " ++ l
    | TAMInst.CALL l => "CALL " ++ l
    | TAMInst.RETURN x y => "RETURN " ++ toString x ++ " " ++ toString y
    | TAMInst.LOAD mode op a =>
        "LOAD [" ++ toString mode ++ " " ++ toString op ++ " " ++ toString a ++ "]"
    | TAMInst.STORE mode op a =>
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

def getLabel : St Int Label := do
  let n ← stState
  stUpdate (n + 1)
  return "#" ++ toString n

mutual
  partial def binOpCode (inst : List TAMInst) (v : VarEnv) (x y : Expr) : St Int (List TAMInst) := do
    let xcode ← expCode v x
    let ycode ← expCode v y
    return (xcode ++ ycode ++ inst)

  partial def unOpCode (inst : List TAMInst) (v : VarEnv) (x : Expr) : St Int (List TAMInst) := do
    let xcode ← expCode v x
    return (xcode ++ inst)

  partial def expCode (v : VarEnv) (e : Expr) : St Int (List TAMInst) :=
    match e with
    | Expr.LitInteger x => return [TAMInst.LOADL x]
    | Expr.LitBool x => return [TAMInst.LOADL (fromBool x)]
    | Expr.Identifier x =>
        return (match varAddr v x with
          | (StackMode.StackBase, offset) => [TAMInst.LOAD StackMode.StackBase AddressOperation.Plus offset]
          | (StackMode.LocalBase, offset) => [TAMInst.LOAD StackMode.LocalBase AddressOperation.Minus offset])

    | Expr.BinOp BinOperator.Addition x y => binOpCode [TAMInst.ADD] v x y
    | Expr.BinOp BinOperator.Subtraction x y => binOpCode [TAMInst.SUB] v x y
    | Expr.BinOp BinOperator.Multiplication x y => binOpCode [TAMInst.MUL] v x y
    | Expr.BinOp BinOperator.Division x y => binOpCode [TAMInst.DIV] v x y

    | Expr.BinOp BinOperator.BinAnd x y => binOpCode [TAMInst.AND] v x y
    | Expr.BinOp BinOperator.BinOr x y => binOpCode [TAMInst.OR] v x y

    | Expr.BinOp BinOperator.Less x y => binOpCode [TAMInst.LSS] v x y
    | Expr.BinOp BinOperator.LessEq x y => binOpCode [TAMInst.LOADL 1, TAMInst.ADD, TAMInst.LSS] v x y

    | Expr.BinOp BinOperator.Greater x y => binOpCode [TAMInst.GRT] v x y
    | Expr.BinOp BinOperator.GreaterEq x y => binOpCode [TAMInst.LOADL 1, TAMInst.SUB, TAMInst.GRT] v x y

    | Expr.BinOp BinOperator.Equals x y => binOpCode [TAMInst.EQL] v x y
    | Expr.BinOp BinOperator.NotEquals x y => binOpCode [TAMInst.EQL, TAMInst.NOT] v x y

    | Expr.UnOp UnOperator.Negation x => unOpCode [TAMInst.NEG] v x
    | Expr.UnOp UnOperator.UnNot x => unOpCode [TAMInst.NOT] v x

    | Expr.Conditional cond yes no => do
        let condcode ← expCode v cond
        let yescode ← expCode v yes
        let l1 ← getLabel
        let nocode ← expCode v no
        let l2 ← getLabel
        return (condcode ++
                [TAMInst.JUMPIFZ l1] ++
                yescode ++
                [TAMInst.JUMP l2, TAMInst.LABEL l1] ++
                nocode ++
                [TAMInst.LABEL l2])

    | Expr.FunctionCall id args => do
        let argscode ← List.flatten <$> args.mapM (expCode v)
        return (argscode ++ [TAMInst.CALL id])
end

partial def compCmd (v : VarEnv) (cmd : Command) : St Int (List TAMInst) :=
  match cmd with
  | Command.Assignment x expr => do
      let ecode ← expCode v expr
      let store := match varAddr v x with
        | (StackMode.StackBase, offset) => [TAMInst.STORE StackMode.StackBase AddressOperation.Plus offset]
        | (StackMode.LocalBase, offset) => [TAMInst.STORE StackMode.LocalBase AddressOperation.Minus offset]
      return (ecode ++ store)

  | Command.IfThenElse expr yes no => do
      let ecode ← expCode v expr
      let yescode ← compCmd v yes
      let l1 ← getLabel
      let nocode ← compCmd v no
      let l2 ← getLabel
      return (ecode ++
              [TAMInst.JUMPIFZ l1] ++
              yescode ++
              [TAMInst.JUMP l2, TAMInst.LABEL l1] ++
              nocode ++
              [TAMInst.LABEL l2])

  | Command.While expr cmd => do
      let ecode ← expCode v expr
      let l1 ← getLabel
      let cmdCode ← compCmd v cmd
      let l2 ← getLabel
      return ([TAMInst.LABEL l1] ++
              ecode ++
              [TAMInst.JUMPIFZ l2] ++
              cmdCode ++
              [TAMInst.JUMP l1, TAMInst.LABEL l2])

  | Command.GetInt x =>
      return (match varAddr v x with
        | (StackMode.StackBase, offset) => [TAMInst.GETINT, TAMInst.STORE StackMode.StackBase AddressOperation.Plus offset]
        | (StackMode.LocalBase, offset) => [TAMInst.GETINT, TAMInst.STORE StackMode.LocalBase AddressOperation.Minus offset])

  | Command.PrintInt expr => do
      let ecode ← expCode v expr
      return (ecode ++ [TAMInst.PUTINT])

  | Command.BeginEnd [] => return []
  | Command.BeginEnd (c :: cs) => do
      let ccode ← compCmd v c
      let cscode ← compCmd v (Command.BeginEnd cs)
      return (ccode ++ cscode)

partial def compileVars (inst : List TAMInst) (ds : List Declaration) (v : VarEnv) : St Int (VarEnv × List TAMInst) :=
  match ds with
  | [] => return (v, inst)
  | Declaration.VarDecl id _ty :: rest =>
      let v' := (id, (StackMode.StackBase, (Int.ofNat v.length))) :: v
      compileVars (inst ++ [TAMInst.LOADL 0]) rest v'
  | Declaration.VarInit id _ty e :: rest => do
      let v' := (id, (StackMode.StackBase, (Int.ofNat v.length))) :: v
      let ecode ← expCode v e
      compileVars (inst ++ ecode) rest v'
  | Declaration.FunctionDecl _ _ _ _ :: rest => compileVars inst rest v

def compileFunction (v : VarEnv) : Declaration → St Int (List TAMInst)
  | Declaration.FunctionDecl id args _ret e => do
      let v' := args.reverse.zipWith (fun (arg, _) i => (arg, (StackMode.LocalBase, i)))
                                      (List.range' 1 args.length) ++ v
      let ecode ← expCode v' e
      return ([TAMInst.LABEL id] ++ ecode ++ [TAMInst.RETURN 1 (Int.ofNat args.length)])
  | _ => return []

def compileProgram : Program → St Int (List TAMInst)
  | Program.LetIn decls cmd => do
      let (v, initCode) ← compileVars [] decls []
      let cmdCode ← compCmd v cmd
      let funcCode ← List.flatten <$> decls.mapM (compileFunction v)
      return (initCode ++ cmdCode ++ [TAMInst.HALT] ++ funcCode)

def compileMT (s : String) : List TAMInst × Error :=
  let ast := parseMT s
  let err := (app (checkProgram ast) []).1
  let inst := if err == "" then (app (compileProgram ast) 0).1 else []
  (inst, err)
