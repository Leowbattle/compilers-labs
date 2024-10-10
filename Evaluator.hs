module Evaluator where

import Scanner
import Parser

evaluate :: AST -> Integer

evaluate (LitInteger i) = i

evaluate (BinOp Addition a b) = (evaluate a) + (evaluate b)
evaluate (BinOp Subtraction a b) = (evaluate a) - (evaluate b)
evaluate (BinOp Multiplication a b) = (evaluate a) * (evaluate b)
evaluate (BinOp Division a b) = (evaluate a) `div` (evaluate b)
evaluate (BinOp Modulus a b) = (evaluate a) `rem` (evaluate b)

evaluate (BinOp BinAnd a b) = let a' = evaluate a
                                  b' = evaluate b in
                                    case (a' /= 0, b' /= 0) of
                                        (False, False) -> 0
                                        (False, True) -> 0
                                        (True, False) -> 0
                                        (True, True) -> 1

evaluate (BinOp BinOr a b) = let a' = evaluate a
                                 b' = evaluate b in
                                    case (a' /= 0, b' /= 0) of
                                        (False, False) -> 0
                                        (False, True) -> 1
                                        (True, False) -> 1
                                        (True, True) -> 1
                                    

evaluate (UnOp Negation a) = - (evaluate a)
evaluate (UnOp AbsoluteValue a) = abs (evaluate a)
evaluate (UnOp UnNot a) = let a' = evaluate a in case a' of
    0 -> 1
    _ -> 0

eval :: String -> Integer
eval s = evaluate $ parse $ scan s
