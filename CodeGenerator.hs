module CodeGenerator where

import Parser
import Scanner

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
    deriving (Show, Read)

expCode :: AST -> [TAMInst]
expCode (LitInteger x) = [LOADL (fromIntegral x)]

expCode (BinOp Addition x y) = (expCode x) ++ (expCode y) ++ [ADD]
expCode (BinOp Subtraction x y) = (expCode x) ++ (expCode y) ++ [SUB]
expCode (BinOp Multiplication x y) = (expCode x) ++ (expCode y) ++ [MUL]
expCode (BinOp Division x y) = (expCode x) ++ (expCode y) ++ [DIV]

expCode (BinOp BinAnd x y) = (expCode x) ++ (expCode y) ++ [AND]
expCode (BinOp BinOr x y) = (expCode x) ++ (expCode y) ++ [OR]

expCode (UnOp Negation x) = (expCode x) ++ [NEG]
expCode (UnOp AbsoluteValue x) = (expCode x) ++ [NEG]
expCode (UnOp UnNot x) = (expCode x) ++ [NOT]

expCode x = error $ show x

compArith :: String -> [TAMInst]
compArith s = let ((ast, _):_) = parse expr s in expCode ast
