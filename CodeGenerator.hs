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

binOpCode :: [TAMInst] -> VarEnv -> Expr -> Expr -> ST [TAMInst]
binOpCode inst v x y = do xcode <- expCode v x
                          ycode <- expCode v y
                          return $ xcode ++ ycode ++ inst

unOpCode :: [TAMInst] -> VarEnv -> Expr -> ST [TAMInst]
unOpCode inst v x = do xcode <- expCode v x
                       return $ xcode ++ inst

expCode :: VarEnv -> Expr -> ST [TAMInst]

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

compCmd v (Assign x expr) = do ecode <- expCode v expr
                               return $ ecode ++ [STORE $ varAddr v x]

compCmd v (If expr yes no) = do  ecode <- expCode v expr
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

-- compCmd v s = let ((c, _):_) = parse command s in app $ compCmd v c

commCode :: VarEnv -> Command -> [TAMInst]
commCode v c = fst $ app (compCmd v c) 0

compileVars :: [TAMInst] -> [Declaration] -> VarEnv -> ST (VarEnv, [TAMInst])
compileVars inst [] v = return (v, inst)
compileVars inst ((Var id e):ds) v = do let v' = (id, length v):v
                                        ecode <- expCode v e
                                        compileVars (inst ++ ecode) ds v'

compileProgram :: Program -> ST [TAMInst]
compileProgram (Program decls cmd) = do (v, initCode) <- compileVars [] decls []
                                        cmdCode <- compCmd v cmd
                                        return $ initCode ++ cmdCode ++ [HALT]

compileMT :: String -> [TAMInst]
compileMT s = fst $ app (compileProgram $ parseMT s) 0
