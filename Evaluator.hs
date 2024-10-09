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

evaluate (UnOp Negation a) = - (evaluate a)
evaluate (UnOp AbsoluteValue a) = abs (evaluate a)

eval :: String -> Integer
eval s = evaluate $ parse $ scan s
