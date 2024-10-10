module TAMVirtualMachine where

import Parser
import CodeGenerator
import Control.Monad (foldM)

-- Virtual machine

execute :: Stack -> TAMInst -> Stack
execute s (LOADL x) = x : s

execute (x:y:s) ADD = x + y : s
execute (x:y:s) SUB = y - x : s
execute (x:y:s) MUL = x * y : s
execute (x:y:s) DIV = y `div` x : s

execute (x:y:s) AND = case (x, y) of
    (_, 0) -> 0 : s
    (0, _) -> 0 : s
    (_, _) -> 1 : s

execute (x:y:s) OR = case (x, y) of
    (0, 0) -> 0 : s
    (_, _) -> 1 : s

execute (x:s) NEG = -x : s

execute (x:s) NOT = case x of
    0 -> 1 : s
    _ -> 0 : s

execTAM :: Stack -> [TAMInst] -> Stack
execTAM s [] = s
execTAM s (i:is) = execTAM (execute s i) is



-- traceTAM

traceInstr :: Stack -> TAMInst -> IO Stack
traceInstr s i = do
    let s' = execute s i
    putStrLn (show i ++ "\t\t" ++ show s')
    pure s'

traceTAM :: Stack -> [TAMInst] -> IO Stack
traceTAM s is = do
    putStrLn ("Initial stack: " ++ show s)
    foldM traceInstr s is
