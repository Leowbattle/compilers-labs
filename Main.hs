import Scanner
import Parser
import TAMVirtualMachine
import CodeGenerator

import System.Environment
import System.FilePath.Posix(takeBaseName, takeExtension)

main :: IO ()
main = do
    args <- getArgs
    let srcFile = head args
    case (takeExtension srcFile) of
        ".exp" -> runCompiler srcFile
        ".tam" -> runInterpreter srcFile

runCompiler :: String -> IO ()
runCompiler srcFile = do
    src <- readFile srcFile
    let code = compArith src
    let outPath = (takeBaseName srcFile) ++ ".tam"
    writeFile outPath (show code)

runInterpreter :: String -> IO ()
runInterpreter srcFile = do
    src <- readFile srcFile
    let code = read src :: [TAMInst]
    let result = execTAM [] code
    print result
