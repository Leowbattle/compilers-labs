/-
  Direct port of `TypeChecker.hs`.
-/

import MTParser
import Parser
import State

abbrev VarContext := List (String × VFType)
abbrev Error := String

def pExpr (s : String) : Expr :=
  match parse expr s with
  | (e, _) :: _ => e
  | [] => panic! "pExpr: no parse"

mutual
  partial def typeCheckBinOp (vars : VarContext) (x y : Expr) (targ tret : MTType) : Option VFType := do
    let tx ← typeCheckExpr vars x
    let ty ← typeCheckExpr vars y
    if tx == ty && tx == VFType.VarType targ then
      return (VFType.VarType tret)
    else
      none

  partial def typeCheckExpr (vars : VarContext) (e : Expr) : Option VFType :=
    match e with
    | Expr.Identifier id => List.lookup id vars
    | Expr.LitInteger _ => some (VFType.VarType MTType.TyInt)
    | Expr.LitBool _ => some (VFType.VarType MTType.TyBool)

    | Expr.BinOp BinOperator.Addition x y => typeCheckBinOp vars x y MTType.TyInt MTType.TyInt
    | Expr.BinOp BinOperator.Subtraction x y => typeCheckBinOp vars x y MTType.TyInt MTType.TyInt
    | Expr.BinOp BinOperator.Multiplication x y => typeCheckBinOp vars x y MTType.TyInt MTType.TyInt
    | Expr.BinOp BinOperator.Division x y => typeCheckBinOp vars x y MTType.TyInt MTType.TyInt

    | Expr.BinOp BinOperator.BinAnd x y => typeCheckBinOp vars x y MTType.TyBool MTType.TyBool
    | Expr.BinOp BinOperator.BinOr x y => typeCheckBinOp vars x y MTType.TyBool MTType.TyBool

    | Expr.BinOp BinOperator.Less x y => typeCheckBinOp vars x y MTType.TyInt MTType.TyBool
    | Expr.BinOp BinOperator.LessEq x y => typeCheckBinOp vars x y MTType.TyInt MTType.TyBool
    | Expr.BinOp BinOperator.Greater x y => typeCheckBinOp vars x y MTType.TyInt MTType.TyBool
    | Expr.BinOp BinOperator.GreaterEq x y => typeCheckBinOp vars x y MTType.TyInt MTType.TyBool

    | Expr.BinOp BinOperator.Equals x y => do
        let tx ← typeCheckExpr vars x
        let ty ← typeCheckExpr vars y
        if tx == ty then
          return (VFType.VarType MTType.TyBool)
        else
          none

    | Expr.BinOp BinOperator.NotEquals x y => do
        let tx ← typeCheckExpr vars x
        let ty ← typeCheckExpr vars y
        if tx == ty then
          return (VFType.VarType MTType.TyBool)
        else
          none

    | Expr.UnOp UnOperator.Negation x => do
        let tx ← typeCheckExpr vars x
        if tx == VFType.VarType MTType.TyInt then
          return (VFType.VarType MTType.TyInt)
        else
          none

    | Expr.UnOp UnOperator.UnNot x => do
        let tx ← typeCheckExpr vars x
        if tx == VFType.VarType MTType.TyBool then
          return (VFType.VarType MTType.TyBool)
        else
          none

    | Expr.Conditional cond x y => do
        let tcond ← typeCheckExpr vars cond
        let tx ← typeCheckExpr vars x
        let ty ← typeCheckExpr vars y
        if tcond == VFType.VarType MTType.TyBool && tx == ty then
          return tx
        else
          none

    | Expr.FunctionCall id args => do
        let targs ← args.mapM (typeCheckExpr vars)
        match List.lookup id vars with
        | some (VFType.FunType argtypes ret) =>
            if argtypes.map VFType.VarType == targs then
              return (VFType.VarType ret)
            else
              none
        | _ => none
end

def checkDuplicateDecl (vars : VarContext) (d : Declaration) : Error :=
  let name := declName d
  let alreadyDeclared := (vars.map Prod.fst).contains name
  if alreadyDeclared then
    "Variable " ++ name ++ ": Identifier " ++ name ++ " declared more than once\n"
  else
    ""

def checkVarInit (vars : VarContext) : Declaration → Error
  | Declaration.VarInit id ty expr =>
      match typeCheckExpr vars expr with
      | none => "Variable " ++ id ++ ": Expression doesn't type check\n"
      | some texpr =>
          if texpr == VFType.VarType ty then
            ""
          else
            "Variable " ++ id ++ ": Type of expression is different from type of variable\n"
  | _ => ""

def checkFunction (vars : VarContext) : Declaration → Error
  | Declaration.FunctionDecl id args ret expr =>
      match typeCheckExpr (args.map (fun (name, t) => (name, VFType.VarType t)) ++ vars) expr with
      | none => "Function " ++ id ++ ": Expression doesn't type check\n"
      | some texpr =>
          if texpr == VFType.VarType ret then
            ""
          else
            "Function " ++ id ++ ": Type of expression is different from return type of function\n"
  | _ => ""

partial def checkDecls : List Declaration → St VarContext Error
  | [] => pure ""
  | d :: ds =>
      match d with
      | Declaration.FunctionDecl _ _ _ _ => do
          let vars ← stState
          let name := declName d
          stUpdate ((name, declType d) :: vars)
          let vars ← stState
          let err := checkFunction vars d
          let errs ← checkDecls ds
          return (err ++ errs)
      | _ => do
          let vars ← stState
          let name := declName d
          let err := checkDuplicateDecl vars d ++ checkVarInit vars d
          stUpdate ((name, declType d) :: vars)
          let errs ← checkDecls ds
          return (err ++ errs)

def checkCmd (vars : VarContext) : Command → Error
  | Command.Assignment id expr =>
      match List.lookup id vars with
      | none => "Assignment: Variable " ++ id ++ " does not exist\n"
      | some ty =>
          match typeCheckExpr vars expr with
          | none => "Assignment: Variable " ++ id ++ " expression does not type check\n"
          | some texpr =>
              if texpr == ty then ""
              else "Assignment: Variable " ++ id ++ " expression has wrong type\n"

  | Command.IfThenElse cond a b =>
      match typeCheckExpr vars cond with
      | none => "If command: test expression does not type check\n"
      | some (VFType.VarType MTType.TyInt) =>
          "If command: test expression is not a boolean\n" ++ checkCmd vars a ++ checkCmd vars b
      | some (VFType.VarType MTType.TyBool) => checkCmd vars a ++ checkCmd vars b
      | some (VFType.FunType _ _) => panic! "checkCmd: unexpected function type"

  | Command.While cond cmd =>
      match typeCheckExpr vars cond with
      | none => "While command: test expression does not type check\n"
      | some (VFType.VarType MTType.TyInt) => "While command: test expression is not a boolean\n"
      | some (VFType.VarType MTType.TyBool) => checkCmd vars cmd
      | some (VFType.FunType _ _) => panic! "checkCmd: unexpected function type"

  | Command.GetInt id =>
      match List.lookup id vars with
      | none => "GetInt command: Variable " ++ id ++ " not found\n"
      | some (VFType.VarType MTType.TyBool) => "GetInt command: Variable " ++ id ++ " not Integer\n"
      | some (VFType.VarType MTType.TyInt) => ""
      | some (VFType.FunType _ _) => panic! "checkCmd: unexpected function type"

  | Command.PrintInt expr =>
      match typeCheckExpr vars expr with
      | none => "PrintInt command: Expression does not type check\n"
      | some (VFType.VarType MTType.TyBool) => "PrintInt command: Expression must have type Integer\n"
      | some (VFType.VarType MTType.TyInt) => ""
      | some (VFType.FunType _ _) => panic! "checkCmd: unexpected function type"

  | Command.BeginEnd cmds => String.join (cmds.map (checkCmd vars))

def checkProgram : Program → St VarContext Error
  | Program.LetIn ds cmd => do
      let ed ← checkDecls ds
      let vars ← stState
      let ec := checkCmd vars cmd
      return (ed ++ ec)

def getDecls (s : String) : List Declaration :=
  match parseMT s with
  | Program.LetIn ds _ => ds
