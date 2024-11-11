module MTParser where

import Parser
import Control.Applicative (Alternative, empty, many, (<|>))
import Data.Char

data Program = LetIn [Declaration] Command
               deriving Show

type Identifier = String
data Declaration = VarDecl Identifier | VarInit Identifier Expr
                   deriving Show

data Command = Assignment Identifier Expr
             | IfThenElse Expr Command Command
             | While Expr Command
             | GetInt Identifier
             | PrintInt Expr
             | BeginEnd [Command]
             deriving Show

data Expr = Identifier Identifier
    | LitInteger Integer
    | BinOp BinOperator Expr Expr
    | UnOp UnOperator Expr
    | Conditional Expr Expr Expr
    deriving Show
data BinOperator = Addition | Subtraction | Multiplication | Division | BinAnd | BinOr |
                   Less | LessEq | Greater | GreaterEq | Equals | NotEquals
    deriving Show
data UnOperator = Negation | UnNot
    deriving Show

int :: Parser Expr
int =  LitInteger . toInteger <$> natural

operator :: [(String, BinOperator)] -> Parser Expr -> Parser Expr
operator ops p = do x <- p
                    xs <- many $ foldl (<|>) empty $ map (\(sym, op) -> do string sym
                                                                           m <- p
                                                                           return (op, m)) ops
                    return (foldl (\a (op, m) -> BinOp op a m) x xs)

expr :: Parser Expr
expr = condExp <|> logicExp

condExp :: Parser Expr
condExp = do cond <- logicExp
             token $ string "?"
             x <- logicExp
             token $ string ":"
             y <- expr
             return (Conditional cond x y)

logicExp :: Parser Expr
logicExp = do token $ string "!"
              e <- expr
              return (UnOp UnNot e)
           <|>
           operator [("&&", BinAnd), ("||", BinOr)] compExp

compExp :: Parser Expr
compExp = operator [("<", Less),
                    ("<=", LessEq),
                    (">", Greater),
                    (">=", GreaterEq),
                    ("==", Equals),
                    ("!=", NotEquals)] addExp

addExp :: Parser Expr
addExp = operator [("+", Addition), ("-", Subtraction)] mexp

mexp :: Parser Expr
mexp = operator [("*", Multiplication), ("/", Division)] term

term :: Parser Expr
term = token $ int <|>
               Identifier <$> identifier <|>

               do token $ string "-"
                  t <- term
                  return (UnOp Negation t)

               <|>

               do token $ string "("
                  e <- expr
                  token $ string ")"
                  return e


program :: Parser Program
program = do token $ string "let"
             ds <- declarations
             token $ string "in"
             c <- command
             return (LetIn ds c)

declarations :: Parser [Declaration]
declarations = do d <- declaration
                  token $ string ";"
                  ds <- declarations
                  return (d:ds)
               <|>
               do
                  d <- declaration
                  return [d]

declaration :: Parser Declaration
declaration = do token $ string "var"
                 id <- identifier
                 token $ string ":="
                 e <- expr
                 return (VarInit id e)

               <|>

               do token $ string "var"
                  id <- identifier
                  return (VarDecl id)

identifier :: Parser Identifier
identifier = do c <- satisfy item isAlpha
                cs <- many $ satisfy item isAlphaNum
                return (c:cs)

command :: Parser Command
command = do token $ string "if"
             e <- expr
             token $ string "then"
             yes <- command
             token $ string "else"
             no <- command
             return (IfThenElse e yes no)

          <|>

          do token $ string "while"
             e <- expr
             token $ string "do"
             c <- command
             return (While e c)

          <|>

          do token $ string "getint"
             token $ string "("
             id <- identifier
             token $ string ")"
             return (GetInt id)

          <|>

          do token $ string "printint"
             token $ string "("
             e <- expr
             token $ string ")"
             return (PrintInt e)

          <|>

          do token $ string "begin"
             cs <- commands
             token $ string "end"
             return (BeginEnd cs)

          <|>

          do id <- identifier
             token $ string ":="
             e <- expr
             return (Assignment id e)

commands :: Parser [Command]
commands =     do c <- command
                  token $ string ";"
                  cs <- commands
                  return (c:cs)
               <|>
               do
                  c <- command
                  return [c]

parseMT :: String -> Program
parseMT s = let ((t, _):_) = parse program s in t
