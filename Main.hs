import Parser
import MTParser
import TAMVirtualMachine
import CodeGenerator

import System.Environment ( getArgs )

takeExtension :: String -> String
takeExtension = dropWhile (/= '.')

removeExtension :: String -> String
removeExtension = takeWhile (/= '.')

main :: IO ()
main = do
    args <- getArgs
    let srcFile = head args
    let trace = "--trace" `elem` args
    case (takeExtension srcFile) of
        ".mt" -> runCompiler srcFile
        ".tam" -> runInterpreter srcFile trace

runCompiler :: String -> IO ()
runCompiler srcFile = do
    src <- readFile srcFile
    let code = compileProgram $ parseMT src
    let outPath = (removeExtension srcFile) ++ ".tam"
    writeFile outPath (show code)

runInterpreter :: String -> Bool -> IO ()
runInterpreter srcFile trace = do
    src <- readFile srcFile
    let code = read src :: [TAMInst]
    (result, _) <- if trace then
        traceTAM [] 0 code
    else
        execTAM [] 0 code
    -- (result, _) <- traceTAM [] 0 code
    putStr "Final stack: "
    print result
