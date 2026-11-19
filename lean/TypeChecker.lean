/-
  Direct port of `TypeChecker.hs`.
-/

import MTParser
import Parser

open MTType VFType Command Declaration Program BinOperator UnOperator
-- Excludes `Identifier`, which would otherwise shadow the `Identifier` type alias.
open Expr (LitInteger LitBool BinOp UnOp Conditional FunctionCall)

abbrev VarContext := List (String × VFType)
abbrev Error := String

def typeCheckBinOp (rec : Expr → Option VFType)
    (x y : Expr) (targ tret : MTType) : Option VFType := do
  let tx ← rec x
  let ty ← rec y
  if tx == ty && tx == VarType targ then
    return (VarType tret)
  else
    none

def typeCheckBody (vars : VarContext) (rec : Expr → Option VFType) :
    Expr → Option VFType
  | Expr.Identifier id => List.lookup id vars
  | LitInteger _ => some (VarType TyInt)
  | LitBool _ => some (VarType TyBool)

  | BinOp Addition x y => typeCheckBinOp rec x y TyInt TyInt
  | BinOp Subtraction x y => typeCheckBinOp rec x y TyInt TyInt
  | BinOp Multiplication x y => typeCheckBinOp rec x y TyInt TyInt
  | BinOp Division x y => typeCheckBinOp rec x y TyInt TyInt

  | BinOp BinAnd x y => typeCheckBinOp rec x y TyBool TyBool
  | BinOp BinOr x y => typeCheckBinOp rec x y TyBool TyBool

  | BinOp Less x y => typeCheckBinOp rec x y TyInt TyBool
  | BinOp LessEq x y => typeCheckBinOp rec x y TyInt TyBool
  | BinOp Greater x y => typeCheckBinOp rec x y TyInt TyBool
  | BinOp GreaterEq x y => typeCheckBinOp rec x y TyInt TyBool

  | BinOp Equals x y => do
      let tx ← rec x
      let ty ← rec y
      if tx == ty then
        return (VarType TyBool)
      else
        none

  | BinOp NotEquals x y => do
      let tx ← rec x
      let ty ← rec y
      if tx == ty then
        return (VarType TyBool)
      else
        none

  | UnOp Negation x => do
      let tx ← rec x
      if tx == VarType TyInt then
        return (VarType TyInt)
      else
        none

  | UnOp UnNot x => do
      let tx ← rec x
      if tx == VarType TyBool then
        return (VarType TyBool)
      else
        none

  | Conditional cond x y => do
      let tcond ← rec cond
      let tx ← rec x
      let ty ← rec y
      if tcond == VarType TyBool && tx == ty then
        return tx
      else
        none

  | FunctionCall id args => do
      let targs ← args.mapM rec
      match List.lookup id vars with
      | some (FunType argtypes ret) =>
          if argtypes.map VarType == targs then
            return (VarType ret)
          else
            none
      | _ => none

def exprSize : Expr → Nat
  | Expr.Identifier _ => 1
  | LitInteger _ => 1
  | LitBool _ => 1
  | BinOp _ x y => 1 + exprSize x + exprSize y
  | UnOp _ x => 1 + exprSize x
  | Conditional c x y => 1 + exprSize c + exprSize x + exprSize y
  | FunctionCall _ args => 1 + (args.map exprSize).foldl (· + ·) 0

def typeCheckFix (vars : VarContext) : Expr → Option VFType
  | e =>
      typeCheckBody vars
        (fun x => if _h : exprSize x < exprSize e then typeCheckFix vars x else none)
        e
termination_by e => exprSize e

def typeCheckExpr (vars : VarContext) (e : Expr) : Option VFType :=
  typeCheckFix vars e

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

def checkDecls : List Declaration → StateM VarContext Error
  | [] => pure ""
  | d :: ds =>
      match d with
      | FunctionDecl _ _ _ _ => do
          let vars ← get
          let name := declName d
          set ((name, declType d) :: vars)
          let vars ← get
          let err := checkFunction vars d
          let errs ← checkDecls ds
          return (err ++ errs)
      | _ => do
          let vars ← get
          let name := declName d
          let err := checkDuplicateDecl vars d ++ checkVarInit vars d
          set ((name, declType d) :: vars)
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
      | some (FunType _ _) => "Command: function used where a value is expected\n"

  | While cond cmd =>
      match typeCheckExpr vars cond with
      | none => "While command: test expression does not type check\n"
      | some (VarType TyInt) => "While command: test expression is not a boolean\n"
      | some (VarType TyBool) => checkCmd vars cmd
      | some (FunType _ _) => "Command: function used where a value is expected\n"

  | GetInt id =>
      match List.lookup id vars with
      | none => "GetInt command: Variable " ++ id ++ " not found\n"
      | some (VarType TyBool) => "GetInt command: Variable " ++ id ++ " not Integer\n"
      | some (VarType TyInt) => ""
      | some (FunType _ _) => "Command: function used where a value is expected\n"

  | PrintInt expr =>
      match typeCheckExpr vars expr with
      | none => "PrintInt command: Expression does not type check\n"
      | some (VarType TyBool) => "PrintInt command: Expression must have type Integer\n"
      | some (VarType TyInt) => ""
      | some (FunType _ _) => "Command: function used where a value is expected\n"

  | BeginEnd cmds => String.join (cmds.map (checkCmd vars))

def checkProgram : Program → StateM VarContext Error
  | LetIn ds cmd => do
      let ed ← checkDecls ds
      let vars ← get
      let ec := checkCmd vars cmd
      return (ed ++ ec)
