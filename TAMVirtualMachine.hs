module TAMVirtualMachine where

import Parser
import CodeGenerator
import Control.Monad (foldM)

-- Virtual machine

execute :: Stack -> TAMInst -> IO Stack
execute s (LOADL x) = return $ x : s

execute (x:y:s) ADD = return $ x + y : s
execute (x:y:s) SUB = return $ y - x : s
execute (x:y:s) MUL = return $ x * y : s
execute (x:y:s) DIV = return $ y `div` x : s

execute (x:y:s) LSS = return $ (if x < y then 1 else 0) : s
execute (x:y:s) GRT = return $ (if x > y then 1 else 0) : s
execute (x:y:s) EQL = return $ (if x == y then 1 else 0) : s

execute (x:y:s) AND = return $ case (x, y) of
    (_, 0) -> 0 : s
    (0, _) -> 0 : s
    (_, _) -> 1 : s

execute (x:y:s) OR = return $ case (x, y) of
    (0, 0) -> 0 : s
    (_, _) -> 1 : s

execute (x:s) NEG = return $ -x : s

execute (x:s) NOT = return $ case x of
    0 -> 1 : s
    _ -> 0 : s

execute s HALT = error "halt" -- TODO

execute s GETINT = do x <- getLine
                      return (read x : s)

execute (x:s) PUTINT = print x >> pure s

execute s (LABEL l) = return s

execute s (JUMP l) = error "JUMP unimplemented"
execute s (JUMPIFZ l) = error "JUMPIFZ unimplemented"

execute s (LOAD a) = return $ varLookup s a : s
execute (x:s) (STORE a) = return $ let (as, _:bs) = splitAt a s in
                                       as ++ [x] ++ bs

varLookup :: Stack -> Int -> Int
varLookup s a = s !! (length s - a - 1)

execTAM :: Stack -> [TAMInst] -> IO Stack
execTAM s is = foldM execute s is



-- traceTAM

traceInstr :: Stack -> TAMInst -> IO Stack
traceInstr s i = do
    let s' = execute s i
    stack <- s'
    putStrLn (show i ++ "\t\t" ++ show stack)
    s'

traceTAM :: Stack -> [TAMInst] -> IO Stack
traceTAM s is = do
    putStrLn ("Initial stack: " ++ show s)
    foldM traceInstr s is
