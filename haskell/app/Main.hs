import Parser
import MTParser
import TAMParser
import TAMVirtualMachine
import CodeGenerator
import TypeChecker

import System.Environment ( getArgs )

takeExtension :: String -> String
takeExtension s = reverse $ takeWhile (/= '.') $ reverse s

removeExtension :: String -> String
removeExtension s = reverse $ tail $ dropWhile (/= '.') $ reverse s

main :: IO ()
main = do
    args <- getArgs
    let srcFile = head args
    let trace = "--trace" `elem` args
    case takeExtension srcFile of
        "mt" -> runCompiler srcFile
        "tam" -> runInterpreter srcFile trace

runCompiler :: String -> IO ()
runCompiler srcFile = do
    src <- readFile srcFile
    let (code, error) = compileMT src
    let outPath = (removeExtension srcFile) ++ ".tam"
    putStr (if error == "" then "" else error ++ "\n")
    putStrLn $ "compiled to TAM file: " ++ outPath
    writeFile outPath (showTAM code)

runInterpreter :: String -> Bool -> IO ()
runInterpreter srcFile trace = do
    src <- readFile srcFile
    let code = parseTAM src
    result <- if trace then
        traceTAM code
    else
        runTAM code
    putStr "Final stack: "
    print result
