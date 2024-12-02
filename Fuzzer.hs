import Text.Printf
import Data.List
import System.Random
import Control.Monad
import MTParser

import State

randUniform :: (Int, Int) -> ST StdGen Int
randUniform r = do gen <- stState
                   let (n, gen') = uniformR r gen
                   stUpdate gen'
                   return n

randSelect :: [a] -> ST StdGen a
randSelect xs = do i <- randUniform (0, length xs - 1)
                   return (xs !! i)

letters = ['a'..'z']

randLetter :: ST StdGen Char
randLetter = randSelect letters

randList :: (Int, Int) -> ST StdGen a -> ST StdGen [a]
randList r g = do n <- randUniform r
                  replicateM n g

randPair :: ST StdGen a -> ST StdGen b -> ST StdGen (a, b)
randPair a b = (,) <$> a <*> b

randString :: ST StdGen String
randString = randList (1, 10) randLetter

randIdentifier :: ST StdGen Expr
randIdentifier = Identifier <$> randString

randLitInteger :: ST StdGen Expr
randLitInteger = LitInteger . toInteger <$> randUniform (-100, 100)

randLitBool :: ST StdGen Expr
randLitBool = LitBool <$> randSelect [False, True]

randBinOp :: ST StdGen Expr
randBinOp = do a <- randExpr
               b <- randExpr
               op <- randSelect [Addition, Subtraction, Multiplication, Division, BinAnd, BinOr, Less, LessEq, Greater, GreaterEq, Equals, NotEquals]
               return $ BinOp op a b

randUnOp :: ST StdGen Expr
randUnOp = do a <- randExpr
              op <- randSelect [Negation, UnNot]
              return $ UnOp op a

randConditional :: ST StdGen Expr
randConditional = Conditional <$> randExpr <*> randExpr <*> randExpr

randFunctionCall :: ST StdGen Expr
randFunctionCall = do id <- randString
                      args <- randList (0, 2) randExpr
                      return $ FunctionCall id args

randExpr :: ST StdGen Expr
randExpr = join $ randSelect [
    randIdentifier,
    randLitInteger,
    randLitBool,
    randBinOp,
    randUnOp,
    randConditional,
    randFunctionCall]

randType :: ST StdGen Type
randType = randSelect [TyInt, TyBool]

randVarDecl :: ST StdGen Declaration
randVarDecl = VarDecl <$> randString <*> randType

randVarInit :: ST StdGen Declaration
randVarInit = VarInit <$> randString <*> randType <*> randExpr

randFunctionDecl :: ST StdGen Declaration
randFunctionDecl = do id <- randString
                      args <- randList (0, 2) (randPair randString randType)
                      ret <- randType
                      expr <- randExpr
                      return $ FunctionDecl id args ret expr

randDecl :: ST StdGen Declaration
randDecl = join $ randSelect [randVarDecl, randVarInit, randFunctionDecl]

randDecls :: ST StdGen [Declaration]
randDecls = randList (1, 5) randDecl

randAssignment = Assignment <$> randString <*> randExpr
randIf = IfThenElse <$> randExpr <*> randCommand <*> randCommand
randWhile = While <$> randExpr <*> randCommand
randGetInt = GetInt <$> randString
randPrintInt = PrintInt <$> randExpr
randBeginEnd = BeginEnd <$> randList (1, 5) randCommand

randCommand :: ST StdGen Command
randCommand = join $ randSelect [
    randAssignment,
    randIf,
    randWhile,
    randGetInt,
    randPrintInt,
    randBeginEnd]

randProgram :: ST StdGen Program
randProgram = do ds <- randDecls
                 c <- randCommand
                 return $ LetIn ds c

main :: IO ()
main = do seed <- randomIO :: IO Int
          let g = mkStdGen seed
              (p, _) = app randProgram g
              ps = showProg p

          print p
          print ps
          let p' = parseMT ps
          print p'
        --   if p /= p' then do putStrLn "Error: parsed program does not match original program"
        --                      print p
        --                      putStrLn "\n"
        --                      print p'
        --                      putStrLn "\n"
        --                      putStrLn ps
        --     else
        --         putStrLn "ok"
          return ()

showProg :: Program -> String
showProg (LetIn decls cmd) = let (c, _) = app (showCmd cmd) 0 in printf "let\n%s\nin\n%s\n" (showDecls decls) c

showOp :: BinOperator -> String
showOp Addition       = "+"
showOp Subtraction    = "-"
showOp Multiplication = "*"
showOp Division       = "/"
showOp BinAnd         = "&&"
showOp BinOr          = "||"
showOp Less           = "<"
showOp LessEq         = "<="
showOp Greater        = ">"
showOp GreaterEq      = ">="
showOp Equals         = "=="
showOp NotEquals      = "!="

showUnOp :: UnOperator -> String
showUnOp Negation = "-"
showUnOp UnNot = "!"

showExpr :: Expr -> String
showExpr (Identifier id) = "(" ++ id ++ ")"
showExpr (LitInteger x) = "(" ++ show x ++ ")"
showExpr (LitBool False) = "(false)"
showExpr (LitBool True) = "(true)"
showExpr (BinOp op x y) = printf "(%s %s %s)" (showExpr x) (showOp op) (showExpr y)
showExpr (UnOp op x) = printf "(%s%s)" (showUnOp op) (showExpr x)
showExpr (Conditional c a b) = printf "(%s ? %s : %s)" (showExpr c) (showExpr a) (showExpr b)
showExpr (FunctionCall id args) = printf "(%s(%s))" id (intercalate ", " (map showExpr args))

showType TyInt = "Integer"
showType TyBool = "Boolean"

showArgs :: [(Identifier, Type)] -> String
showArgs args = intercalate ", " (map (\(id, t) -> printf "%s: %s" id (showType t)) args)

showDecl (VarDecl id t) = printf "var %s: %s" id (showType t)
showDecl (VarInit id t e) = printf "var %s: %s := %s" id (showType t) (showExpr e)
showDecl (FunctionDecl id args ret e) = printf "fun %s(%s): %s = %s" id (showArgs args) (showType ret) (showExpr e)

showDecls :: [Declaration] -> String
showDecls decls = intercalate ";\n" $ map (("\t" ++) . showDecl) decls

indented :: ST Int String -> ST Int String
indented x = do n <- stState
                s <- x
                return $ replicate n '\t' ++ s

indent :: ST Int ()
indent = do n <- stState
            stUpdate (n + 1)

deindent :: ST Int ()
deindent = do n <- stState
              stUpdate (n - 1)

showCmd :: Command -> ST Int String
showCmd (Assignment id e) = indented $ return $ printf "%s := %s" id (showExpr e)
showCmd (IfThenElse c a b) = do s1 <- indented $ return $ printf "if %s then\n" (showExpr c)
                                indent
                                s2 <- showCmd a
                                deindent
                                s3 <- indented $ return $ printf "\nelse\n"
                                indent
                                s4 <- showCmd b
                                deindent
                                return $ s1 ++ s2 ++ s3 ++ s4

showCmd (While c a) = do s1 <- indented $ return $ printf "while %s do\n" (showExpr c)
                         indent
                         s2 <- showCmd a
                         deindent
                         return $ s1 ++ s2

showCmd (GetInt id) = indented $ return $ printf "getint(%s)" id
showCmd (PrintInt e) = indented $ return $ printf "printint(%s)" (showExpr e)
showCmd (BeginEnd cs) = do s1 <- indented $ return "begin\n"
                           indent
                           s2 <- intercalate ";\n" <$> mapM showCmd cs
                           deindent
                           s3 <- indented $ return "\nend"
                           return $ s1 ++ s2 ++ s3
