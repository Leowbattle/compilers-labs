import Data.Char (isSpace, digitToInt, isNumber)
import Debug.Trace (trace)

data Token = IntLiteral Integer -- Numbers
    | Oper Operator -- Operators (no arity distinction)
    | OpenPar | ClosedPar -- Parentheses
    deriving Show

data Operator = Plus | Minus | Times | Divide
    deriving Show

scan :: String -> [Token]

scan "" = []
scan ('+' : s) = Oper Plus : scan s
scan ('-' : s) = Oper Minus : scan s
scan ('*' : s) = Oper Times : scan s
scan ('/' : s) = Oper Divide : scan s

scan ('(' : s) = OpenPar : scan s
scan (')' : s) = ClosedPar : scan s

scan (c : s)
    | isSpace c = scan s
    | isNumber c = let (i, s') = scanInt (c : s) 0 in
        IntLiteral i : scan s'

scanInt :: String -> Integer -> (Integer, String)
scanInt "" x = (x, "")
scanInt (c : s) x
    | isNumber c = let (i, s') = scanInt s (10 * x + toInteger (digitToInt c)) in (i, s')
    | otherwise = (x, c : s)

data AST = LitInteger Integer
    | BinOp BinOperator AST AST
    | UnOp UnOperator AST
    deriving Show
data BinOperator = Addition | Subtraction | Multiplication | Division
    deriving Show
data UnOperator = Negation
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

            ts'' -> (Just t', ts')

term :: [Token] -> (Maybe AST, [Token])
term [] = (Nothing, [])
term (IntLiteral i : ts) = (Just (LitInteger i), ts)
term (Oper Minus : ts) = let (t, ts') = expr ts in
    case t of
        Nothing -> (Nothing, Oper Minus : ts)
        Just t' -> (Just (UnOp Negation t'), ts')

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

evaluate (UnOp Negation a) = - (evaluate a)

eval :: String -> Integer
eval s = evaluate $ parse $ scan s
