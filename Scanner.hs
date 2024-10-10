module Scanner where

import Data.Char (isSpace, digitToInt, isNumber, isAlphaNum)

data Token = IntLiteral Integer -- Numbers
    | StrLiteral String
    | Oper Operator -- Operators (no arity distinction)
    | OpenPar | ClosedPar -- Parentheses
    deriving Show

data Operator = Plus | Minus | Times | Divide | Modulo | Abs | And | Or | Not
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

scan ('&' : '&' : s) = Oper And : scan s
scan ('|' : '|' : s) = Oper Or : scan s
scan ('!' : s) = Oper Not : scan s

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
