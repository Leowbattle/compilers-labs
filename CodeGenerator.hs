module CodeGenerator where

import Parser

type Stack = [Int]

data TAMInst
    = LOADL Int -- push Integer into the stack
    | ADD -- adds two top values in the stack
    | SUB -- subtract second element of stack from top
    | MUL -- multiplies top values in the stack
    | DIV -- divides the second value by the top
    | NEG -- negates the top of the stack
    deriving (Show, Read)

expCode :: AST -> [TAMInst]
expCode (LitInteger x) = [LOADL (fromIntegral x)]

expCode (BinOp Addition x y) = (expCode x) ++ (expCode y) ++ [ADD]
expCode (BinOp Subtraction x y) = (expCode x) ++ (expCode y) ++ [SUB]
expCode (BinOp Multiplication x y) = (expCode x) ++ (expCode y) ++ [MUL]
expCode (BinOp Division x y) = (expCode x) ++ (expCode y) ++ [DIV]

expCode (UnOp Negation x) = (expCode x) ++ [NEG]

expCode x = error $ show x
