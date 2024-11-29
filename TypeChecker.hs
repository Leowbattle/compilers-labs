module TypeChecker where

import MTParser
import Parser
import State

type VarContext = [(String, Type)]
type Error = String

checkDecls :: [Declaration] -> ST VarContext Error
checkDecls [] = pure []
checkDecls (d:ds) = do vars <- stState
                       let name = declName d
                           exists = declName d `elem` map fst vars
                           err = if exists then
                            "Variable " ++ name ++ ": Identifier " ++ name ++ " declared more than once\n"
                           else
                            ""
                       stUpdate ((name, declType d):vars)
                       errs <- checkDecls ds
                       return (err ++ errs)

checkProgram :: Program -> ST VarContext Error
checkProgram = undefined

getDecls :: String -> [Declaration]
getDecls s = let (LetIn ds _) = parseMT s in ds
