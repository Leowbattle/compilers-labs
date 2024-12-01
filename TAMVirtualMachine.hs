module TAMVirtualMachine where

import Parser
import State
import CodeGenerator
import Control.Monad (foldM, replicateM, replicateM_)
import Data.List

data TAMState = TAMState {
    tsCode :: [TAMInst],
    tsCounter :: Int,
    tsStack :: Stack,
    tsSB :: Int,
    tsLB :: Int
}

type TAMSt a = StateIO TAMState a

tsPush :: Int -> TAMState -> TAMState
tsPush x ts = ts {tsStack = x : tsStack ts}

tsPop :: TAMState -> (Int,TAMState)
tsPop ts = let (x:xs) = tsStack ts
            in (x,ts{tsStack = xs})

tsSetCounter :: Int -> TAMState -> TAMState
tsSetCounter i ts = ts {tsCounter = i}

tsSetLB :: Int -> TAMState -> TAMState
tsSetLB lb ts = ts { tsLB = lb }

tsSetLBT :: Int -> TAMSt ()
tsSetLBT lb = do state <- stStateT
                 stUpdateT (tsSetLB lb state)

tsInstruction :: TAMState -> TAMInst
tsInstruction ts = tsCode ts !! tsCounter ts

instructionT :: TAMSt TAMInst
instructionT = do (TAMState tsCode _ _ _ _) <- stStateT
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

jumpToLabel :: Label -> StateIO TAMState ()
jumpToLabel l = do (TAMState tsCode _ _ _ _) <- stStateT
                   countUpdateT (lookupLabel tsCode l)

varLookup :: StackMode -> AddressOperation -> Int -> StateIO TAMState Int
varLookup mode op a = do (TAMState _ _ tsStack sb lb) <- stStateT
                         let base = if mode == StackBase then sb else lb
                             offset = if op == Plus then a else -a
                         return $ length tsStack - (base + offset) - 1

getVar :: StackMode -> AddressOperation -> Int -> StateIO TAMState Int
getVar mode op a = do (TAMState _ _ tsStack sb lb) <- stStateT
                      addr <- varLookup mode op a
                      return $ tsStack !! addr

setVar :: Int -> Int -> StateIO TAMState ()
setVar a x = do ts <- stStateT
                let (TAMState _ _ tsStack _ _) = ts
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

executeT LSS = do y <- popT
                  x <- popT
                  pushT (if x < y then 1 else 0)
                  continueT

executeT GRT = do y <- popT
                  x <- popT
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

-- executeT (LOAD mode op a) = error "LOAD not implemented"
-- executeT (LOAD mode op a) = do let x = 0
--                                pushT x
--                                continueT

executeT (LOAD mode op a) = do x <- getVar mode op a
                               pushT x
                               continueT

-- executeT (STORE mode op a) = error "STORE not implemented"
-- executeT (STORE mode op a) = do x <- popT
--                                 addr <- varLookup a
--                                 setVar addr x
--                                 continueT

executeT (STORE mode op a) = do x <- popT
                                addr <- varLookup mode op a
                                setVar addr x
                                continueT

executeT (CALL l) = do (TAMState _ pc stack sb lb) <- stStateT
                       pushT lb
                       pushT (pc + 1)
                       tsSetLBT (length stack)
                       jumpToLabel l

executeT (RETURN n m) = do (TAMState _ pc stack sb lb) <- stStateT
                           retVal <- replicateM n popT
                           ra <- popT
                           lb <- popT
                           replicateM_ m popT
                           mapM_ pushT retVal
                           countUpdateT ra

lookupLabel :: [TAMInst] -> Label -> Int
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
    _ -> 0

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
initialState prog = TAMState {tsCode = prog, tsCounter = 0, tsStack = [], tsSB = 0, tsLB = 0}

runTAM :: [TAMInst] -> IO Stack
runTAM prog = do (_, TAMState _ _ tsStack _ _) <- appT execTAM (initialState prog)
                 return tsStack

traceTAM' :: StateIO TAMState ()
traceTAM' = do inst <- instructionT
               executeT inst
               (TAMState prog pc s' _ _) <- stStateT
               pc <- counterT
               ts <- stStateT
               if pc == -1 then
                  return ()
               else do
                  lift $ putStrLn (show (prog !! pc) ++ "\t\t" ++ show s')
                  traceTAM'

traceTAM :: [TAMInst] -> IO Stack
traceTAM prog = do (_, TAMState _ _ tsStack _ _) <- appT traceTAM' (initialState prog)
                   return tsStack
