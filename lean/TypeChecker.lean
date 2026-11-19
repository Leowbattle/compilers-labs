/-
  Direct port of `TypeChecker.hs`.
-/

import MTParser
import Parser
import State

open MTType VFType Command Declaration Program BinOperator UnOperator
-- Excludes `Identifier`, which would otherwise shadow the `Identifier` type alias.
open Expr (LitInteger LitBool BinOp UnOp Conditional FunctionCall)

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
    if tx == ty && tx == VarType targ then
      return (VarType tret)
    else
      none

  partial def typeCheckExpr (vars : VarContext) (e : Expr) : Option VFType :=
    match e with
    | Expr.Identifier id => List.lookup id vars
    | LitInteger _ => some (VarType TyInt)
    | LitBool _ => some (VarType TyBool)

    | BinOp Addition x y => typeCheckBinOp vars x y TyInt TyInt
    | BinOp Subtraction x y => typeCheckBinOp vars x y TyInt TyInt
    | BinOp Multiplication x y => typeCheckBinOp vars x y TyInt TyInt
    | BinOp Division x y => typeCheckBinOp vars x y TyInt TyInt

    | BinOp BinAnd x y => typeCheckBinOp vars x y TyBool TyBool
    | BinOp BinOr x y => typeCheckBinOp vars x y TyBool TyBool

    | BinOp Less x y => typeCheckBinOp vars x y TyInt TyBool
    | BinOp LessEq x y => typeCheckBinOp vars x y TyInt TyBool
    | BinOp Greater x y => typeCheckBinOp vars x y TyInt TyBool
    | BinOp GreaterEq x y => typeCheckBinOp vars x y TyInt TyBool

    | BinOp Equals x y => do
        let tx ← typeCheckExpr vars x
        let ty ← typeCheckExpr vars y
        if tx == ty then
          return (VarType TyBool)
        else
          none

    | BinOp NotEquals x y => do
        let tx ← typeCheckExpr vars x
        let ty ← typeCheckExpr vars y
        if tx == ty then
          return (VarType TyBool)
        else
          none

    | UnOp Negation x => do
        let tx ← typeCheckExpr vars x
        if tx == VarType TyInt then
          return (VarType TyInt)
        else
          none

    | UnOp UnNot x => do
        let tx ← typeCheckExpr vars x
        if tx == VarType TyBool then
          return (VarType TyBool)
        else
          none

    | Conditional cond x y => do
        let tcond ← typeCheckExpr vars cond
        let tx ← typeCheckExpr vars x
        let ty ← typeCheckExpr vars y
        if tcond == VarType TyBool && tx == ty then
          return tx
        else
          none

    | FunctionCall id args => do
        let targs ← args.mapM (typeCheckExpr vars)
        match List.lookup id vars with
        | some (FunType argtypes ret) =>
            if argtypes.map VarType == targs then
              return (VarType ret)
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
  | VarInit id ty expr =>
      match typeCheckExpr vars expr with
      | none => "Variable " ++ id ++ ": Expression doesn't type check\n"
      | some texpr =>
          if texpr == VarType ty then
            ""
          else
            "Variable " ++ id ++ ": Type of expression is different from type of variable\n"
  | _ => ""

def checkFunction (vars : VarContext) : Declaration → Error
  | FunctionDecl id args ret expr =>
      match typeCheckExpr (args.map (fun (name, t) => (name, VarType t)) ++ vars) expr with
      | none => "Function " ++ id ++ ": Expression doesn't type check\n"
      | some texpr =>
          if texpr == VarType ret then
            ""
          else
            "Function " ++ id ++ ": Type of expression is different from return type of function\n"
  | _ => ""

partial def checkDecls : List Declaration → St VarContext Error
  | [] => pure ""
  | d :: ds =>
      match d with
      | FunctionDecl _ _ _ _ => do
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
  | Assignment id expr =>
      match List.lookup id vars with
      | none => "Assignment: Variable " ++ id ++ " does not exist\n"
      | some ty =>
          match typeCheckExpr vars expr with
          | none => "Assignment: Variable " ++ id ++ " expression does not type check\n"
          | some texpr =>
              if texpr == ty then ""
              else "Assignment: Variable " ++ id ++ " expression has wrong type\n"

  | IfThenElse cond a b =>
      match typeCheckExpr vars cond with
      | none => "If command: test expression does not type check\n"
      | some (VarType TyInt) =>
          "If command: test expression is not a boolean\n" ++ checkCmd vars a ++ checkCmd vars b
      | some (VarType TyBool) => checkCmd vars a ++ checkCmd vars b
      | some (FunType _ _) => panic! "checkCmd: unexpected function type"

  | While cond cmd =>
      match typeCheckExpr vars cond with
      | none => "While command: test expression does not type check\n"
      | some (VarType TyInt) => "While command: test expression is not a boolean\n"
      | some (VarType TyBool) => checkCmd vars cmd
      | some (FunType _ _) => panic! "checkCmd: unexpected function type"

  | GetInt id =>
      match List.lookup id vars with
      | none => "GetInt command: Variable " ++ id ++ " not found\n"
      | some (VarType TyBool) => "GetInt command: Variable " ++ id ++ " not Integer\n"
      | some (VarType TyInt) => ""
      | some (FunType _ _) => panic! "checkCmd: unexpected function type"

  | PrintInt expr =>
      match typeCheckExpr vars expr with
      | none => "PrintInt command: Expression does not type check\n"
      | some (VarType TyBool) => "PrintInt command: Expression must have type Integer\n"
      | some (VarType TyInt) => ""
      | some (FunType _ _) => panic! "checkCmd: unexpected function type"

  | BeginEnd cmds => String.join (cmds.map (checkCmd vars))

def checkProgram : Program → St VarContext Error
  | LetIn ds cmd => do
      let ed ← checkDecls ds
      let vars ← stState
      let ec := checkCmd vars cmd
      return (ed ++ ec)

def getDecls (s : String) : List Declaration :=
  match parseMT s with
  | LetIn ds _ => ds
