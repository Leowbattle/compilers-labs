module CodeGenerator where

import Parser
import MTParser
import Data.List (find, intercalate)
import State

type Stack = [Int]

type Label = String

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

    | LOAD Int
    | STORE Int
    deriving (Show, Read)

showTAM :: [TAMInst] -> String
showTAM prog = intercalate "\n" (map show prog)

type VarEnv = [(String, Int)]

fromJust :: Maybe a -> a
fromJust Nothing = error "Nothing"
fromJust (Just x) = x

varAddr :: VarEnv -> String -> Int
varAddr v x = snd $ fromJust (find ((== x) . fst) v)

binOpCode :: [TAMInst] -> VarEnv -> Expr -> Expr -> ST Int [TAMInst]
binOpCode inst v x y = do xcode <- expCode v x
                          ycode <- expCode v y
                          return $ xcode ++ ycode ++ inst

unOpCode :: [TAMInst] -> VarEnv -> Expr -> ST Int [TAMInst]
unOpCode inst v x = do xcode <- expCode v x
                       return $ xcode ++ inst

expCode :: VarEnv -> Expr -> ST Int [TAMInst]

expCode v (LitInteger x) = return [LOADL (fromIntegral x)]
expCode v (Identifier x) = return [LOAD $ varAddr v x]

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
                                         

-- expCode v x = error $ show x

-- compExpr v s = let ((e, _):_) = parse expr s in expCode v e

getLabel :: ST Int Label
getLabel = do n <- stState
              stUpdate (n + 1)
              return $ "#" ++ show n

compCmd :: VarEnv -> Command -> ST Int [TAMInst]

compCmd v (Assignment x expr) = do ecode <- expCode v expr
                                   return $ ecode ++ [STORE $ varAddr v x]

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

compCmd v (GetInt x) = return [GETINT, STORE $ varAddr v x]
compCmd v (PrintInt expr) = do ecode <- expCode v expr
                               return $ ecode ++ [PUTINT]

compCmd v (BeginEnd []) = return []
compCmd v (BeginEnd (c:cs)) = do  ccode <- compCmd v c
                                  cscode <- compCmd v (BeginEnd cs)
                                  return $ ccode ++ cscode

compileVars :: [TAMInst] -> [Declaration] -> VarEnv -> ST Int (VarEnv, [TAMInst])
compileVars inst [] v = return (v, inst)
compileVars inst ((VarDecl id ty):ds) v = do let v' = (id, length v):v
                                          -- ecode <- expCode v e
                                             compileVars (inst ++ [LOADL 0]) ds v'
compileVars inst ((VarInit id ty e):ds) v = do let v' = (id, length v):v
                                               ecode <- expCode v e
                                               compileVars (inst ++ ecode) ds v'

compileProgram :: Program -> ST Int [TAMInst]
compileProgram (LetIn decls cmd) = do (v, initCode) <- compileVars [] decls []
                                      cmdCode <- compCmd v cmd
                                      return $ initCode ++ cmdCode ++ [HALT]

compileMT :: String -> [TAMInst]
compileMT s = fst $ app (compileProgram $ parseMT s) 0
