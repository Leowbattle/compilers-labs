module CodeGenerator where

import Parser
import MTParser
import State
import TypeChecker
import Data.List (find, intercalate)

-- TODO Remove
import Debug.Trace

type Stack = [Int]

type Label = String

data StackMode = StackBase | LocalBase deriving Eq

instance Show StackMode where
  show StackBase = "SB"
  show LocalBase = "LB"

data AddressOperation = Plus | Minus deriving Eq

instance Show AddressOperation where
  show Plus = "+"
  show Minus = "-"

data TAMInst
    = LOADL Int -- push Integer into the stack
    | ADD -- adds two top values in the stack
    | SUB -- subtract second element of stack from top
    | MUL -- multiplies top values in the stack
    | DIV -- divides the second value by the top
    | NEG -- negates the top of the stack

    | AND -- Boolean and
    | OR -- Boolean or
    | NOT -- Boolean not

    | LSS
    | GRT
    | EQL

    | HALT
    | GETINT
    | PUTINT

    | LABEL Label
    | JUMP Label
    | JUMPIFZ Label
    | CALL Label
    | RETURN Int Int

    | LOAD StackMode AddressOperation Int
    | STORE StackMode AddressOperation Int
--     deriving (Show, Read)

instance Show TAMInst where
  show (LOADL x) = "LOADL " ++ show x
  show ADD = "ADD"
  show SUB = "SUB"
  show MUL = "MUL"
  show DIV = "DIV"
  show NEG = "NEG"
  show AND = "AND"
  show OR = "OR"
  show NOT = "NOT"
  show LSS = "LSS"
  show GRT = "GRT"
  show EQL = "EQL"
  show HALT = "HALT"
  show GETINT = "GETINT"
  show PUTINT = "PUTINT"
  show (LABEL l) = "LABEL " ++ l
  show (JUMP l) = "JUMP " ++ l
  show (JUMPIFZ l) = "JUMPIFZ " ++ l
  show (CALL l) = "CALL " ++ l
  show (RETURN x y) = "RETURN " ++ show x ++ " " ++ show y
  show (LOAD mode op a) = "LOAD [" ++ show mode ++ " " ++ show op ++ " " ++ show a ++ "]"
  show (STORE mode op a) = "STORE [" ++ show mode ++ " " ++ show op ++ " " ++ show a ++ "]"

showTAM :: [TAMInst] -> String
showTAM prog = intercalate "\n" (map show prog)

type VarEnv = [(String, (StackMode, Int))]

fromJust :: Maybe a -> a
fromJust Nothing = error "fromJust Nothing"
fromJust (Just x) = x

fromBool :: Bool -> Int
fromBool False = 0
fromBool True = 1

varAddr :: VarEnv -> String -> (StackMode, Int)
-- varAddr v x = trace (show v ++ "   " ++ show x) $ fromJust $ lookup x v
varAddr v x = fromJust $ lookup x v

binOpCode :: [TAMInst] -> VarEnv -> Expr -> Expr -> ST Int [TAMInst]
binOpCode inst v x y = do xcode <- expCode v x
                          ycode <- expCode v y
                          return $ xcode ++ ycode ++ inst

unOpCode :: [TAMInst] -> VarEnv -> Expr -> ST Int [TAMInst]
unOpCode inst v x = do xcode <- expCode v x
                       return $ xcode ++ inst

expCode :: VarEnv -> Expr -> ST Int [TAMInst]

expCode v (LitInteger x) = return [LOADL (fromIntegral x)]
expCode v (LitBool x) = return [LOADL (fromBool x)]
expCode v (Identifier x) = return $ case varAddr v x of
        (StackBase, offset) -> [LOAD StackBase Plus offset]
        (LocalBase, offset) -> [LOAD LocalBase Minus offset]

expCode v (BinOp Addition x y) = binOpCode [ADD] v x y
expCode v (BinOp Subtraction x y) = binOpCode [SUB] v x y
expCode v (BinOp Multiplication x y) = binOpCode [MUL] v x y
expCode v (BinOp Division x y) = binOpCode [DIV] v x y

expCode v (BinOp BinAnd x y) = binOpCode [AND] v x y
expCode v (BinOp BinOr x y) = binOpCode [OR] v x y

expCode v (BinOp Less x y) = binOpCode [LSS] v x y
expCode v (BinOp LessEq x y) = binOpCode [LOADL 1, ADD, LSS] v x y

expCode v (BinOp Greater x y) = binOpCode [GRT] v x y
expCode v (BinOp GreaterEq x y) = binOpCode [LOADL 1, SUB, GRT] v x y

expCode v (BinOp Equals x y) = binOpCode [EQL] v x y
expCode v (BinOp NotEquals x y) = binOpCode [EQL, NOT] v x y

expCode v (UnOp Negation x) = unOpCode [NEG] v x
expCode v (UnOp UnNot x) = unOpCode [NOT] v x

expCode v (Conditional cond yes no) = do condcode <- expCode v cond
                                         yescode <- expCode v yes
                                         l1 <- getLabel
                                         nocode <- expCode v no
                                         l2 <- getLabel
                                         return (condcode ++
                                                 [JUMPIFZ l1] ++
                                                 yescode ++
                                                 [JUMP l2, LABEL l1] ++
                                                 nocode ++
                                                 [LABEL l2])

expCode v (FunctionCall id args) = do argscode <- concat <$> mapM (expCode v) (reverse args)
                                      return $ argscode ++ [CALL id]

-- expCode v x = error $ show x

-- compExpr v s = let ((e, _):_) = parse expr s in expCode v e

getLabel :: ST Int Label
getLabel = do n <- stState
              stUpdate (n + 1)
              return $ "#" ++ show n

compCmd :: VarEnv -> Command -> ST Int [TAMInst]

compCmd v (Assignment x expr) = do ecode <- expCode v expr
                                   let store = case varAddr v x of
                                        (StackBase, offset) -> [STORE StackBase Plus offset]
                                        (LocalBase, offset) -> [STORE LocalBase Minus offset]
                                   return $ ecode ++ store

compCmd v (IfThenElse expr yes no) = do  ecode <- expCode v expr
                                         yescode <- compCmd v yes
                                         l1 <- getLabel
                                         nocode <- compCmd v no
                                         l2 <- getLabel
                                         return (ecode ++
                                                 [JUMPIFZ l1] ++
                                                 yescode ++
                                                 [JUMP l2, LABEL l1] ++
                                                 nocode ++
                                                 [LABEL l2])

compCmd v (While expr cmd) = do  ecode <- expCode v expr
                                 l1 <- getLabel
                                 cmdCode <- compCmd v cmd
                                 l2 <- getLabel
                                 return ([LABEL l1] ++
                                         ecode ++
                                         [JUMPIFZ l2] ++
                                         cmdCode ++
                                         [JUMP l1, LABEL l2])

-- compCmd v (GetInt x) = return [GETINT, STORE $ varAddr v x]
compCmd v (GetInt x) = return $ case varAddr v x of
        (StackBase, offset) -> [GETINT, STORE StackBase Plus offset]
        (LocalBase, offset) -> [GETINT, STORE LocalBase Minus offset]
compCmd v (PrintInt expr) = do ecode <- expCode v expr
                               return $ ecode ++ [PUTINT]

compCmd v (BeginEnd []) = return []
compCmd v (BeginEnd (c:cs)) = do  ccode <- compCmd v c
                                  cscode <- compCmd v (BeginEnd cs)
                                  return $ ccode ++ cscode

compileVars :: [TAMInst] -> [Declaration] -> VarEnv -> ST Int (VarEnv, [TAMInst])
compileVars inst [] v = return (v, inst)
compileVars inst ((VarDecl id ty):ds) v = do let v' = (id, (StackBase, length v)):v
                                          -- ecode <- expCode v e
                                             compileVars (inst ++ [LOADL 0]) ds v'
compileVars inst ((VarInit id ty e):ds) v = do let v' = (id, (StackBase, length v)):v
                                               ecode <- expCode v e
                                               compileVars (inst ++ ecode) ds v'

compileVars inst ((FunctionDecl id args ret e):ds) v = compileVars inst ds v

compileFunction :: VarEnv -> Declaration -> ST Int [TAMInst]
compileFunction v (FunctionDecl id args ret e) = do let v' = zipWith (\(arg, _) i -> (arg, (LocalBase, i))) args [1..] ++ v
                                                    ecode <- expCode v' e
                                                    return $ [LABEL id] ++ ecode ++ [RETURN 1 (length args)]
compileFunction v _ = return []

compileProgram :: Program -> ST Int [TAMInst]
compileProgram (LetIn decls cmd) = do (v, initCode) <- compileVars [] decls []
                                      cmdCode <- compCmd v cmd
                                      funcCode <- concat <$> mapM (compileFunction v) decls
                                      return $ initCode ++ cmdCode ++ [HALT] ++ funcCode

-- compileMT :: String -> [TAMInst]
-- compileMT s = fst $ app (compileProgram $ parseMT s) 0

compileMT :: String -> ([TAMInst], Error)
compileMT s = let ast = parseMT s
                  err = fst $ app (checkProgram ast) []
                  inst = if err == "" then fst $ app (compileProgram ast) 0 else []
                  in (inst, err)
