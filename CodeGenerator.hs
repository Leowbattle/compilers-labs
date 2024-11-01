module CodeGenerator where

import Parser
import MTParser
import Data.List (find)

type Stack = [Int]

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

    | LABEL Int
    | JUMP Int
    | JUMPIFZ Int

    | LOAD Int
    | STORE Int
    deriving (Show, Read)

type VarEnv = [(String, Int)]

fromJust :: Maybe a -> a
fromJust Nothing = error "Nothing"
fromJust (Just x) = x

varAddr :: VarEnv -> String -> Int
varAddr v x = snd $ fromJust (find ((== x) . fst) v)

expCode :: VarEnv -> Expr -> [TAMInst]
expCode v (LitInteger x) = [LOADL (fromIntegral x)]
expCode v (Identifier x) = [LOAD $ varAddr v x]

expCode v (BinOp Addition x y) = (expCode v x) ++ (expCode v y) ++ [ADD]
expCode v (BinOp Subtraction x y) = (expCode v x) ++ (expCode v y) ++ [SUB]
expCode v (BinOp Multiplication x y) = (expCode v x) ++ (expCode v y) ++ [MUL]
expCode v (BinOp Division x y) = (expCode v x) ++ (expCode v y) ++ [DIV]

expCode v (BinOp BinAnd x y) = (expCode v x) ++ (expCode v y) ++ [AND]
expCode v (BinOp BinOr x y) = (expCode v x) ++ (expCode v y) ++ [OR]

expCode v (BinOp Less x y) = (expCode v x) ++ (expCode v y) ++ [LSS]
expCode v (BinOp LessEq x y) = (expCode v x) ++ (expCode v y) ++ [LOADL 1, ADD, LSS]

expCode v (BinOp Greater x y) = (expCode v x) ++ (expCode v y) ++ [GRT]
expCode v (BinOp GreaterEq x y) = (expCode v x) ++ (expCode v y) ++ [LOADL 1, SUB, GRT]

expCode v (BinOp Equals x y) = (expCode v x) ++ (expCode v y) ++ [EQL]
expCode v (BinOp NotEquals x y) = (expCode v x) ++ (expCode v y) ++ [EQL, NOT]

expCode v (UnOp Negation x) = (expCode v x) ++ [NEG]
expCode v (UnOp UnNot x) = (expCode v x) ++ [NOT]

-- expCode v (Conditional cond yes no) = 

expCode v x = error $ show x

-- compExpr v s = let ((e, _):_) = parse expr s in expCode v e





type Label = Int
type State = Label

newtype ST a = S (State -> (a, State))

app :: ST a -> State -> (a,State)
app (S st) x = st x

instance Functor ST where
   -- fmap :: (a -> b) -> ST a -> ST b
   fmap g st = S (\s -> let (x,s') = app st s in (g x, s'))

instance Applicative ST where
   -- pure :: a -> ST a
   pure x = S (\s -> (x,s))

   -- (<*>) :: ST (a -> b) -> ST a -> ST b
   stf <*> stx = S (\s ->
      let (f,s')  = app stf s
          (x,s'') = app stx s' in (f x, s''))

instance Monad ST where
   -- return :: a -> ST a
   return = pure

   -- (>>=) :: ST a -> (a -> ST b) -> ST b
   st >>= f = S (\s -> let (x,s') = app st s in app (f x) s')

getLabel :: ST Int
getLabel = S (\s -> (s, s + 1))




compCmd :: VarEnv -> Command -> ST [TAMInst]
compCmd v (Assign x expr) = return $ (expCode v expr) ++ [STORE $ varAddr v x]

compCmd v (If expr yes no) = do  let ecode = expCode v expr
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

compCmd v (While expr cmd) = do  let ecode = expCode v expr
                                 l1 <- getLabel
                                 cmdCode <- compCmd v cmd
                                 l2 <- getLabel
                                 return ([LABEL l1] ++
                                         ecode ++
                                         [JUMPIFZ l2] ++
                                         cmdCode ++
                                         [JUMP l1, LABEL l2])

compCmd v (GetInt x) = return $ [GETINT, STORE $ varAddr v x]
compCmd v (PrintInt expr) = return $ expCode v expr ++ [PUTINT]

compCmd v (BeginEnd []) = return []
compCmd v (BeginEnd (c:cs)) = do  ccode <- compCmd v c
                                  cscode <- compCmd v (BeginEnd cs)
                                  return $ ccode ++ cscode

-- compCmd v s = let ((c, _):_) = parse command s in app $ compCmd v c

commCode :: VarEnv -> Command -> [TAMInst]
commCode v c = fst $ app (compCmd v c) 0

compileDeclarations :: [Declaration] -> VarEnv
compileDeclarations decls = [(id, i) | (Var id _, i) <- zip decls [0..]]

compileVarInit :: [Declaration] -> [TAMInst]
compileVarInit decls = concat (map (\(Var _ expr) -> expCode [] expr) decls)

compileProgram :: Program -> [TAMInst]
compileProgram (Program decls cmd) = compileVarInit decls ++ commCode (compileDeclarations decls) cmd ++ [HALT]
