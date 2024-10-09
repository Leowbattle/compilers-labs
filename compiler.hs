import Data.Char (isSpace, digitToInt, isNumber, isAlphaNum)
import Debug.Trace (trace)
import System.Environment
import System.FilePath.Posix(takeBaseName)

data Token = IntLiteral Integer -- Numbers
    | StrLiteral String
    | Oper Operator -- Operators (no arity distinction)
    | OpenPar | ClosedPar -- Parentheses
    deriving Show

data Operator = Plus | Minus | Times | Divide | Modulo | Abs
    deriving Show

scan :: String -> [Token]

scan "" = []
scan ('+' : s) = Oper Plus : scan s
scan ('-' : s) = Oper Minus : scan s
scan ('*' : s) = Oper Times : scan s
scan ('/' : s) = Oper Divide : scan s
scan ('%' : s) = Oper Modulo : scan s

scan ('(' : s) = OpenPar : scan s
scan (')' : s) = ClosedPar : scan s

scan (c : s)
    | isSpace c = scan s
    | isNumber c = let (i, s') = scanInt (c : s) 0 in
        IntLiteral i : scan s'
    | isAlphaNum c = let (str, s') = scanStr (c : s) "" in
        StrLiteral str : scan s'

scanInt :: String -> Integer -> (Integer, String)
scanInt "" x = (x, "")
scanInt (c : s) x
    | isNumber c = let (i, s') = scanInt s (10 * x + toInteger (digitToInt c)) in (i, s')
    | otherwise = (x, c : s)

scanStr :: String -> String -> (String, String)
scanStr "" s = (s, "")
scanStr (c : cs) s
    | isAlphaNum c = let (s', cs') = scanStr cs (s ++ [c]) in (s', cs')
    | otherwise = (s, c : cs)

data AST = LitInteger Integer
    | BinOp BinOperator AST AST
    | UnOp UnOperator AST
    deriving Show
data BinOperator = Addition | Subtraction | Multiplication | Division | Modulus
    deriving Show
data UnOperator = Negation | AbsoluteValue
    deriving Show

parse :: [Token] -> AST
parse ts = let (e, ts') = expr ts in
    case e of
        Nothing -> error "Parse error"
        Just e' -> e'

expr :: [Token] -> (Maybe AST, [Token])
expr ts = let (m, ts') = mexpr ts in
    case m of
        Nothing -> (Nothing, ts)
        Just m' -> case ts' of
            -- [] -> (Nothing, ts)
            (Oper Plus : ts'') -> let (t'', ts''') = expr ts'' in
                case t'' of
                    Nothing -> (Nothing, ts)
                    Just t''' -> (Just $ BinOp Addition m' t''', ts''')

            (Oper Minus : ts'') -> let (t'', ts''') = mexpr ts'' in
                case t'' of
                    Nothing -> (Nothing, ts)
                    Just t''' -> (Just $ BinOp Subtraction m' t''', ts''')

            ts'' -> (Just m', ts')

mexpr :: [Token] -> (Maybe AST, [Token])
mexpr ts = let (t, ts') = term ts in
    case t of
        Nothing -> (Nothing, ts)
        Just t' -> case ts' of
            -- [] -> (Nothing, ts)
            (Oper Times : ts'') -> let (t'', ts''') = mexpr ts'' in
                case t'' of
                    Nothing -> (Nothing, ts)
                    Just t''' -> (Just $ BinOp Multiplication t' t''', ts''')

            (Oper Divide : ts'') -> let (t'', ts''') = mexpr ts'' in
                case t'' of
                    Nothing -> (Nothing, ts)
                    Just t''' -> (Just $ BinOp Division t' t''', ts''')

            (Oper Modulo : ts'') -> let (t'', ts''') = mexpr ts'' in
                case t'' of
                    Nothing -> (Nothing, ts)
                    Just t''' -> (Just $ BinOp Modulus t' t''', ts''')

            ts'' -> (Just t', ts')

term :: [Token] -> (Maybe AST, [Token])
term [] = (Nothing, [])
term (IntLiteral i : ts) = (Just (LitInteger i), ts)
term (Oper Minus : ts) = let (t, ts') = expr ts in
    case t of
        Nothing -> (Nothing, Oper Minus : ts)
        Just t' -> (Just (UnOp Negation t'), ts')
term (StrLiteral "abs" : ts) = let (t, ts') = expr ts in
    case t of
        Nothing -> (Nothing, Oper Minus : ts)
        Just t' -> (Just (UnOp AbsoluteValue t'), ts')

term (OpenPar : ts) = let (e, ts') = expr ts in
    case e of
        Nothing -> (Nothing, OpenPar : ts)
        Just e' -> case ts' of
            [] -> (Nothing, OpenPar : ts)
            (ClosedPar : ts'') -> (e, ts'')
            (t : ts'') -> (Nothing, OpenPar : ts)

term ts = (Nothing, ts)

evaluate :: AST -> Integer

evaluate (LitInteger i) = i

evaluate (BinOp Addition a b) = (evaluate a) + (evaluate b)
evaluate (BinOp Subtraction a b) = (evaluate a) - (evaluate b)
evaluate (BinOp Multiplication a b) = (evaluate a) * (evaluate b)
evaluate (BinOp Division a b) = (evaluate a) `div` (evaluate b)
evaluate (BinOp Modulus a b) = (evaluate a) `rem` (evaluate b)

evaluate (UnOp Negation a) = - (evaluate a)
evaluate (UnOp AbsoluteValue a) = abs (evaluate a)

eval :: String -> Integer
eval s = evaluate $ parse $ scan s





-- Code generation


type Stack = [Int]

data TAMInst
    = LOADL Int -- push Integer into the stack
    | ADD -- adds two top values in the stack
    | SUB -- subtract second element of stack from top
    | MUL -- multiplies top values in the stack
    | DIV -- divides the second value by the top
    | NEG -- negates the top of the stack
    deriving Show

expCode :: AST -> [TAMInst]
expCode (LitInteger x) = [LOADL (fromIntegral x)]

expCode (BinOp Addition x y) = (expCode x) ++ (expCode y) ++ [ADD]
expCode (BinOp Subtraction x y) = (expCode x) ++ (expCode y) ++ [SUB]
expCode (BinOp Multiplication x y) = (expCode x) ++ (expCode y) ++ [MUL]
expCode (BinOp Division x y) = (expCode x) ++ (expCode y) ++ [DIV]

expCode (UnOp Negation x) = (expCode x) ++ [NEG]

expCode x = error $ show x




-- Virtual machine

execute :: Stack -> TAMInst -> Stack
execute s (LOADL x) = x : s

execute (x:y:s) ADD = x + y : s
execute (x:y:s) SUB = x - y : s
execute (x:y:s) MUL = x * y : s
execute (x:y:s) DIV = x `div` y : s

execute (x:s) NEG = -x : s

execTAM :: Stack -> [TAMInst] -> Stack
execTAM s [] = s
execTAM s (i:is) = execTAM (execute s i) is




compArith :: String -> [TAMInst]
compArith s = expCode $ parse $ scan s



-- Main

main :: IO ()
main = do
    args <- getArgs
    let srcFile = head args
    src <- readFile srcFile
    let code = compArith src
    let outPath = (takeBaseName srcFile) ++ ".tam"
    putStrLn srcFile
    writeFile outPath (show code)