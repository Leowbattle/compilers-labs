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
term = int <|>
       Identifier <$> identifier <|>

       do string "-"
          t <- term
          return (UnOp Negation t)

       <|>

       do string "("
          e <- expr
          string ")"
          return e

program :: Parser Program
program = do string "let"
             ds <- declarations
             string "in"
             c <- command
             return (Program ds c)

declarations :: Parser [Declaration]
declarations = do d <- declaration
                  string ";"
                  ds <- declarations
                  return (d:ds)
               <|>
               ((:[]) <$> declaration)

declaration :: Parser Declaration
declaration = do string "var"
                 id <- identifier
                 string ":="
                 e <- expr
                 return (Var id e)

               <|>

               do string "var"
                  id <- identifier
                  return (Var id (LitInteger 0))

identifier :: Parser Identifier
identifier = do c <- satisfy item isAlpha
                cs <- many $ satisfy item isAlphaNum
                return (c:cs)

command :: Parser Command
command = do string "if"
             e <- expr
             string "then"
             yes <- command
             string "else"
             no <- command
             return (If e yes no)

          <|>

          do string "while"
             e <- expr
             string "do"
             c <- command
             return (While e c)

          <|>

          do string "getint"
             string "("
             id <- identifier
             string ")"
             return (GetInt id)

          <|>

          do string "begin"
             cs <- commands
             string "end"
             return (BeginEnd cs)

          <|>

          do id <- identifier
             string ":="
             e <- expr
             return (Assign id e)

commands :: Parser [Command]
commands =     do c <- command
                  string ";"
                  cs <- commands
                  return (c:cs)
               <|>
               ((:[]) <$> command)

func =    do string "if"
             e <- expr
             string "then"
             yes <- command
             string "else"
             no <- command
             return (Program [] (If e yes no))

parseTAM :: String -> Program
parseTAM s = let ((t, _):_) = parse program (filter (not . isSpace) s) in t
