module TAMVirtualMachine where

import Parser
import State
import CodeGenerator
import Control.Monad (foldM)
import Data.List

data TAMState = TAMState {
    tsCode :: [TAMInst],
    tsCounter :: Int,
    tsStack :: Stack
}

type TAMSt a = ST TAMState a

tsPush :: Int -> TAMState -> TAMState
tsPush x ts = ts {tsStack = x : (tsStack ts)}

tsPop :: TAMState -> (Int,TAMState)
tsPop ts = let (x:xs) = tsStack ts
            in (x,ts{tsStack = xs})

tsSetCounter :: Int -> TAMState -> TAMState
tsSetCounter i ts = ts {tsCounter = i}

tsInstruction :: TAMState -> TAMInst
tsInstruction ts = (tsCode ts)!!(tsCounter ts)

counterT :: TAMSt Int
counterT = do ts <- stState
              return (tsCounter ts)

countUpdateT :: Int -> TAMSt ()
countUpdateT c = do ts <- stState
                    stUpdate (ts{tsCounter = c})

continueT :: TAMSt ()
continueT = do c <- counterT
               countUpdateT (c+1)

pushT :: Int -> TAMSt ()
pushT x = do ts <- stState
             stUpdate (tsPush x ts)
             return ()

popT :: TAMSt Int
popT = do ts <- stState
          let (x, ts') = tsPop ts
          stUpdate ts
          return x

executeT :: TAMInst -> TAMSt ()
executeT (LOADL x) = do pushT x
                        continueT

executeT ADD = do x <- popT
                  y <- popT
                  pushT (x + y)
                  continueT

executeT SUB = do x <- popT
                  y <- popT
                  pushT (y - x)
                  continueT

executeT MUL = do x <- popT
                  y <- popT
                  pushT (x * y)
                  continueT

executeT DIV = do x <- popT
                  y <- popT
                  pushT (y `div` x)
                  continueT

executeT LSS = do x <- popT
                  y <- popT
                  pushT (if x < y then 1 else 0)
                  continueT

executeT GRT = do x <- popT
                  y <- popT
                  pushT (if x > y then 1 else 0)
                  continueT

executeT EQL = do x <- popT
                  y <- popT
                  pushT (if x == y then 1 else 0)
                  continueT

executeT AND = do x <- popT
                  y <- popT
                  pushT (boolAnd x y)
                  continueT

executeT OR = do x <- popT
                 y <- popT
                 pushT (boolOr x y)
                 continueT

executeT NOT = do x <- popT
                  pushT (boolNot x)
                  continueT

executeT NEG = do x <- popT
                  pushT (- x)
                  continueT

step :: Stack -> [TAMInst] -> Int -> IO (Stack, Int)
step s prog pc = case prog !! pc of
    LOADL x -> return (x : s, pc + 1)

    ADD -> let (x:y:s') = s in return (x + y : s', pc + 1)
    SUB -> let (x:y:s') = s in return (y - x : s', pc + 1)
    MUL -> let (x:y:s') = s in return (x * y : s', pc + 1)
    DIV -> let (x:y:s') = s in return (y `div` x : s', pc + 1)

    LSS -> let (y:x:s') = s in return ((if x < y then 1 else 0) : s', pc + 1)
    GRT -> let (y:x:s') = s in return ((if x > y then 1 else 0) : s', pc + 1)
    EQL -> let (x:y:s') = s in return ((if x == y then 1 else 0) : s', pc + 1)

    AND -> let (x:y:s') = s in return (boolAnd x y : s', pc + 1)
    OR ->  let (x:y:s') = s in return (boolOr x y : s', pc + 1)
    NOT -> let (x:s')   = s in return (boolNot x : s', pc + 1)

    NEG -> let (x:s')   = s in return (-x : s', pc + 1)

    HALT -> return (s, -1)

    GETINT -> do putStrLn "Enter a number: "
                 x <- getLine
                 return (read x : s, pc + 1)

    PUTINT -> let (x:s') = s in putStr "output > " >> print x >> pure (s', pc + 1)
    
    LABEL l -> return (s, pc + 1)

    JUMP l -> return (s, lookupLabel prog l)
    JUMPIFZ l -> let (x:s') = s in return (s', if x == 0 then lookupLabel prog l else pc + 1)

    LOAD a -> return ((s !! (varLookup s a)) : s, pc + 1)

    STORE a -> let (x:s') = s in
                   return (setVar s' (varLookup s' a) x, pc + 1)

lookupLabel :: [TAMInst] -> Int -> Int
lookupLabel prog l = fromJust $ findIndex (\x -> case x of
    LABEL l' -> l == l'
    _ -> False) prog

boolAnd :: Int -> Int -> Int
boolAnd x y = case (x, y) of
    (_, 0) -> 0
    (0, _) -> 0
    (_, _) -> 1

boolOr :: Int -> Int -> Int
boolOr x y = case (x, y) of
    (0, 0) -> 0
    (_, _) -> 1

boolNot :: Int -> Int
boolNot x = case x of
    0 -> 1
    1 -> 0

varLookup :: Stack -> Int -> Int
varLookup s a = length s - a - 1

setVar :: Stack -> Int -> Int -> Stack
setVar s a x = let (as, _:bs) = splitAt a s in
                   as ++ [x] ++ bs

-- execTAM :: Stack -> [TAMInst] -> IO Stack
-- execTAM s is = foldM execute s is

execTAM :: Stack -> Int -> [TAMInst] -> IO (Stack, Int)
execTAM s pc prog = do (s', pc') <- step s prog pc
                       if pc' == -1 then
                           return (s', -1)
                       else
                           execTAM s' pc' prog

-- traceTAM

traceTAM :: Stack -> Int -> [TAMInst] -> IO (Stack, Int)
traceTAM s pc prog = do (s', pc') <- step s prog pc
                        putStrLn (show (prog !! pc) ++ "\t\t" ++ show s')
                        if pc' == -1 then
                           return (s', -1)
                        else
                           traceTAM s' pc' prog
