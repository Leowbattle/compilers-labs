module Parser where

import Scanner

data AST = LitInteger Integer
    | BinOp BinOperator AST AST
    | UnOp UnOperator AST
    deriving Show
data BinOperator = Addition | Subtraction | Multiplication | Division | Modulus | BinAnd | BinOr
    deriving Show
data UnOperator = Negation | AbsoluteValue | UnNot
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
