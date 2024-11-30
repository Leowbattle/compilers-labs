module TypeChecker where

import MTParser
import Parser
import State
import Control.Applicative

type VarContext = [(String, VFType)]
type Error = String

pExpr :: String -> Expr
pExpr s = let ((e,_):_) = parse expr s in e

typeCheckBinOp :: VarContext -> Expr -> Expr -> Type -> Type -> Maybe VFType
typeCheckBinOp vars x y targ tret = do tx <- typeCheckExpr vars x
                                       ty <- typeCheckExpr vars y
                                       if tx == ty && tx == VarType targ then
                                          return $ VarType tret
                                       else
                                          Nothing

typeCheckExpr :: VarContext -> Expr -> Maybe VFType
typeCheckExpr vars (Identifier id) = lookup id vars
typeCheckExpr vars (LitInteger _) = Just $ VarType TyInt
typeCheckExpr vars (LitBool _) = Just $ VarType TyBool

typeCheckExpr vars (BinOp Addition x y) = typeCheckBinOp vars x y TyInt TyInt
typeCheckExpr vars (BinOp Subtraction x y) = typeCheckBinOp vars x y TyInt TyInt
typeCheckExpr vars (BinOp Multiplication x y) = typeCheckBinOp vars x y TyInt TyInt
typeCheckExpr vars (BinOp Division x y) = typeCheckBinOp vars x y TyInt TyInt

typeCheckExpr vars (BinOp BinAnd x y) = typeCheckBinOp vars x y TyBool TyBool
typeCheckExpr vars (BinOp BinOr x y) = typeCheckBinOp vars x y TyBool TyBool

typeCheckExpr vars (BinOp Less x y) = typeCheckBinOp vars x y TyInt TyBool
typeCheckExpr vars (BinOp LessEq x y) = typeCheckBinOp vars x y TyInt TyBool
typeCheckExpr vars (BinOp Greater x y) = typeCheckBinOp vars x y TyInt TyBool
typeCheckExpr vars (BinOp GreaterEq x y) = typeCheckBinOp vars x y TyInt TyBool

typeCheckExpr vars (BinOp Equals x y) = do tx <- typeCheckExpr vars x
                                           ty <- typeCheckExpr vars y
                                           if tx == ty then
                                              return $ VarType TyBool
                                           else
                                              Nothing

-- Could reduce code duplication here?
typeCheckExpr vars (BinOp NotEquals x y) = do tx <- typeCheckExpr vars x
                                              ty <- typeCheckExpr vars y
                                              if tx == ty then
                                                 return $ VarType TyBool
                                              else
                                                 Nothing

typeCheckExpr vars (UnOp Negation x) = do tx <- typeCheckExpr vars x
                                          if tx == VarType TyInt then
                                             return $ VarType TyInt
                                          else
                                             Nothing

typeCheckExpr vars (UnOp UnNot x) = do tx <- typeCheckExpr vars x
                                       if tx == VarType TyBool then
                                          return $ VarType TyBool
                                       else
                                          Nothing

typeCheckExpr vars (Conditional cond x y) = do tcond <- typeCheckExpr vars cond
                                               tx <- typeCheckExpr vars x
                                               ty <- typeCheckExpr vars y
                                               if tcond == VarType TyBool && tx == ty then
                                                  return tx
                                               else
                                                  Nothing

typeCheckExpr vars (FunctionCall id args) = do targs <- mapM (typeCheckExpr vars) args
                                               case lookup id vars of
                                                   Just (FunType argtypes ret) -> if map VarType argtypes == targs then
                                                                                     return $ VarType ret
                                                                                  else
                                                                                     Nothing
                                                   _ -> Nothing

checkDuplicateDecl :: VarContext -> Declaration -> Error
checkDuplicateDecl vars d = let name = declName d
                                exists = declName d `elem` map fst vars
                                in if exists then
                                 "Variable " ++ name ++ ": Identifier " ++ name ++ " declared more than once\n"
                                else
                                 ""

checkVarInit :: VarContext -> Declaration -> Error
checkVarInit vars (VarInit id ty expr) = case typeCheckExpr vars expr of
   Nothing -> "Variable " ++ id ++ ": Expression doesn't type check\n"
   Just texpr -> if texpr == VarType ty then "" else "Variable " ++ id ++ ": Type of expression is different from type of variable\n"

checkVarInit _ _ = ""

checkFunction :: VarContext -> Declaration -> Error
checkFunction vars (FunctionDecl id args ret expr) = case typeCheckExpr vars expr of
   Nothing -> "Function " ++ id ++ ": Expression doesn't type check\n"
   Just texpr -> if texpr == VarType ret then "" else "Function " ++ id ++ ": Type of expression is different from return type of function\n"
checkFunction _ _ = ""

checkDecls :: [Declaration] -> ST VarContext Error
checkDecls [] = pure []
checkDecls (d:ds) = do vars <- stState
                       let name = declName d
                           err = checkDuplicateDecl vars d ++
                                 checkVarInit vars d ++
                                 checkFunction vars d
                       stUpdate ((name, declType d):vars)
                       errs <- checkDecls ds
                       return (err ++ errs)

checkCmd :: VarContext -> Command -> Error
checkCmd vars (Assignment id expr) = case lookup id vars of
   Nothing -> "Assignment: Variable " ++ id ++ " does not exist\n"
   Just ty -> case typeCheckExpr vars expr of
      Nothing -> "Assignment: Variable " ++ id ++ " expression does not type check\n"
      Just texpr -> if texpr == ty then "" else "Assignment: Variable " ++ id ++ " expression has wrong type\n"

checkCmd vars (IfThenElse cond a b) = case typeCheckExpr vars cond of
   Nothing -> "If command: test expression does not type check\n"
   Just (VarType TyInt) -> "If command: test expression is not a boolean\n"
   Just (VarType TyBool) -> checkCmd vars a ++ checkCmd vars b

checkCmd vars (While cond cmd) = case typeCheckExpr vars cond of
   Nothing -> "While command: test expression does not type check\n"
   Just (VarType TyInt) -> "While command: test expression is not a boolean\n"
   Just (VarType TyBool) -> checkCmd vars cmd

checkCmd vars (GetInt id) = case lookup id vars of
   Nothing -> "GetInt command: Variable " ++ id ++ " not found\n"
   Just (VarType TyBool) -> "GetInt command: Variable " ++ id ++ " not Integer\n"
   Just (VarType TyInt) -> ""

checkCmd vars (PrintInt expr) = case typeCheckExpr vars expr of
   Nothing -> "PrintInt command: Expression does not type check\n"
   Just (VarType TyBool) -> "PrintInt command: Expression must have type Integer\n"
   Just (VarType TyInt) -> ""

checkCmd vars (BeginEnd cmds) = concatMap (checkCmd vars) cmds

checkProgram :: Program -> ST VarContext Error
checkProgram (LetIn ds cmd) = do ed <- checkDecls ds
                                 vars <- stState
                                 let ec = checkCmd vars cmd
                                       in return (ed ++ ec)

getDecls :: String -> [Declaration]
getDecls s = let (LetIn ds _) = parseMT s in ds
