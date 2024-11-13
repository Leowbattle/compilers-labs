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

type TAMSt a = StateIO TAMState a

tsPush :: Int -> TAMState -> TAMState
tsPush x ts = ts {tsStack = x : (tsStack ts)}

tsPop :: TAMState -> (Int,TAMState)
tsPop ts = let (x:xs) = tsStack ts
            in (x,ts{tsStack = xs})

tsSetCounter :: Int -> TAMState -> TAMState
tsSetCounter i ts = ts {tsCounter = i}

tsInstruction :: TAMState -> TAMInst
tsInstruction ts = (tsCode ts)!!(tsCounter ts)

instructionT :: TAMSt TAMInst
instructionT = do (TAMState tsCode _ _) <- stStateT
                  pc <- counterT
                  return $ tsCode !! pc

counterT :: TAMSt Int
counterT = do ts <- stStateT
              return (tsCounter ts)

countUpdateT :: Int -> TAMSt ()
countUpdateT c = do ts <- stStateT
                    stUpdateT (ts{tsCounter = c})

continueT :: TAMSt ()
continueT = do c <- counterT
               countUpdateT (c+1)

pushT :: Int -> TAMSt ()
pushT x = do ts <- stStateT
             stUpdateT (tsPush x ts)
             return ()

popT :: TAMSt Int
popT = do ts <- stStateT
          let (x, ts') = tsPop ts
          stUpdateT ts'
          return x

jumpToLabel :: Int -> StateIO TAMState ()
jumpToLabel l = do (TAMState tsCode _ _) <- stStateT
                   countUpdateT (lookupLabel tsCode l)

varLookup :: Int -> StateIO TAMState Int
varLookup a = do (TAMState _ _ tsStack) <- stStateT
                 return $ length tsStack - a - 1

getVar :: Int -> StateIO TAMState Int
getVar a = do (TAMState _ _ tsStack) <- stStateT
              addr <- varLookup a
              return $ tsStack !! addr

setVar :: Int -> Int -> StateIO TAMState ()
setVar a x = do ts <- stStateT
                let (TAMState _ _ tsStack) = ts
                let (as, _:bs) = splitAt a tsStack
                stUpdateT $ ts { tsStack = as ++ [x] ++ bs }

executeT :: TAMInst -> StateIO TAMState ()
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

executeT HALT = countUpdateT (-1)

executeT GETINT = do lift $ putStrLn "Enter a number: "
                     x <- lift getLine
                     pushT (read x)
                     continueT

executeT PUTINT = do lift $ putStr "output > "
                     x <- popT
                     lift $ print x
                     continueT

executeT (LABEL l) = continueT

executeT (JUMP l) = jumpToLabel l

executeT (JUMPIFZ l) = do x <- popT
                          if x == 0 then
                            jumpToLabel l
                          else
                            continueT

executeT (LOAD a) = do x <- getVar a
                       pushT x
                       continueT

executeT (STORE a) = do x <- popT
                        addr <- varLookup a
                        setVar addr x
                        continueT

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

-- execTAM :: Stack -> [TAMInst] -> IO Stack
-- execTAM s is = foldM execute s is

execTAM :: StateIO TAMState ()
execTAM = do inst <- instructionT
             executeT inst
             pc <- counterT
             ts <- stStateT
             if pc == -1 then
                return ()
             else
                execTAM

initialState :: [TAMInst] -> TAMState
initialState prog = TAMState {tsCode = prog, tsCounter = 0, tsStack = []}

runTAM :: [TAMInst] -> IO Stack
runTAM prog = do (_, (TAMState _ _ tsStack)) <- appT execTAM (initialState prog)
                 return tsStack

traceTAM' :: StateIO TAMState ()
traceTAM' = do inst <- instructionT
               executeT inst
               (TAMState prog pc s') <- stStateT
               pc <- counterT
               ts <- stStateT
               if pc == -1 then
                  return ()
               else do
                  lift $ putStrLn (show (prog !! pc) ++ "\t\t" ++ show s')
                  traceTAM'

traceTAM :: [TAMInst] -> IO Stack
traceTAM prog = do (_, (TAMState _ _ tsStack)) <- appT traceTAM' (initialState prog)
                   return tsStack
