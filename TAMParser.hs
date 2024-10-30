module TAMParser where

import Parser
import Control.Applicative (Alternative, empty, many, (<|>))
import Data.Char

data Program = Program [Declaration] Command
               deriving Show

type Identifier = String
data Declaration = Var Identifier Expr deriving Show

data Command = Assign Identifier Expr
             | If Expr Command Command
             | While Expr Command
             | GetInt Identifier
             | PrintInt Expr
             | BeginEnd [Command]
             deriving Show

data Expr = Identifier Identifier
    | LitInteger Integer
    | BinOp BinOperator Expr Expr
    | UnOp UnOperator Expr
    deriving Show
data BinOperator = Addition | Subtraction | Multiplication | Division | BinAnd | BinOr |
                   Less | LessEq | Greater | GreaterEq | Equals | NotEquals
    deriving Show
data UnOperator = Negation | UnNot
    deriving Show

int :: Parser Expr
int =  LitInteger . toInteger <$> natural

expr :: Parser Expr
expr =   do x <- mexp
            xs <- many (do string "+"
                           m <- mexp
                           return (Addition, m)
                        <|>
                        do string "-"
                           m <- mexp
                           return (Subtraction, m))

            return (foldl (\a (op, m) -> BinOp op a m) x xs)

mexp :: Parser Expr
mexp =   do x <- term
            xs <- many (do string "/"
                           m <- term
                           return (Division, m)
                        <|>
                        do string "*"
                           m <- term
                           return (Multiplication, m))

            return (foldl (\a (op, m) -> BinOp op a m) x xs)

term :: Parser Expr
term = do spaces
          t <- int <|>
               Identifier <$> identifier <|>

               do string "-"
                  t <- term
                  return (UnOp Negation t)

               <|>

               do string "("
                  e <- expr
                  string ")"
                  return e
          spaces
          return t        

program :: Parser Program
program = do spaces
             string "let"
             spaces
             ds <- declarations
             spaces
             string "in"
             spaces
             c <- command
             spaces
             return (Program ds c)

declarations :: Parser [Declaration]
declarations = do spaces
                  d <- declaration
                  spaces
                  string ";"
                  spaces
                  ds <- declarations
                  spaces
                  return (d:ds)
               <|>
               do
                  spaces
                  d <- declaration
                  spaces
                  return [d]

declaration :: Parser Declaration
declaration = do spaces
                 string "var"
                 spaces
                 id <- identifier
                 spaces
                 string ":="
                 spaces
                 e <- expr
                 spaces
                 return (Var id e)

               <|>

               do spaces
                  string "var"
                  spaces
                  id <- identifier
                  spaces
                  return (Var id (LitInteger 0))

identifier :: Parser Identifier
identifier = do c <- satisfy item isAlpha
                cs <- many $ satisfy item isAlphaNum
                return (c:cs)

command :: Parser Command
command = do spaces
             string "if"
             spaces
             e <- expr
             spaces
             string "then"
             spaces
             yes <- command
             spaces
             string "else"
             spaces
             no <- command
             spaces
             return (If e yes no)

          <|>

          do spaces
             string "while"
             spaces
             e <- expr
             spaces
             string "do"
             spaces
             c <- command
             spaces
             return (While e c)

          <|>

          do spaces
             string "getint"
             spaces
             string "("
             spaces
             id <- identifier
             spaces
             string ")"
             spaces
             return (GetInt id)

          <|>

          do spaces
             string "printint"
             spaces
             string "("
             spaces
             e <- expr
             spaces
             string ")"
             spaces
             return (PrintInt e)

          <|>

          do spaces
             string "begin"
             spaces
             cs <- commands
             spaces
             string "end"
             spaces
             return (BeginEnd cs)

          <|>

          do spaces
             id <- identifier
             spaces
             string ":="
             spaces
             e <- expr
             spaces
             return (Assign id e)

commands :: Parser [Command]
commands =     do spaces
                  c <- command
                  spaces
                  string ";"
                  spaces
                  cs <- commands
                  spaces
                  return (c:cs)
               <|>
               do
                  spaces
                  c <- command
                  spaces
                  return [c]

parseTAM :: String -> Program
parseTAM s = let ((t, _):_) = parse program s in t
