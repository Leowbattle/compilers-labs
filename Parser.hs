module Parser where

import Scanner
import Control.Applicative (Alternative, empty, (<|>))
import Data.Char

data AST = LitInteger Integer
    | BinOp BinOperator AST AST
    | UnOp UnOperator AST
    deriving Show
data BinOperator = Addition | Subtraction | Multiplication | Division | Modulus
    deriving Show
data UnOperator = Negation | AbsoluteValue
    deriving Show

newtype Parser a = P (String -> [(a,String)])

parse :: Parser a -> String -> [(a,String)]
parse (P p) src = p src

instance Functor Parser where
    -- fmap :: (a → b) → Parser a → Parser b
    fmap f p = P (\src -> map (\(v, src') -> (f v, src'))
                              (parse p src))

instance Applicative Parser where
    -- pure :: a → Parser a
    pure v = P (\src -> [(v,src)])

    -- <∗> :: Parser (a → b) → Parser a → Parser b
    pg <*> px = P (\src -> concat $
                    map (\(g,src') -> parse (fmap g px) src')
                        (parse pg src))

instance Monad Parser where
    -- (>>=) :: Parser a → (a → Parser b) → Parser b
    p >>= f = P (\src -> concat $
                  map (\(v,src') -> parse (f v) src')
                    (parse p src))

instance Alternative Parser where
    -- empty :: Parser a
    empty = P (\src -> [])
    -- (<|>) :: Parser a → Parser a → Parser a
    p <|> q = P (\src -> case parse p src of
                           [] -> parse q src
                           rs -> rs)



item :: Parser Char
item = P (\src -> case src of
                      "" -> []
                      (s:src') -> [(s, src')])

natural :: Parser Int
natural = P (\src -> let (ds, src') = span isDigit src
                  in if ds == "" then []
                                 else [(read ds, src')])

pSpace :: Parser ()
pSpace = P (\src -> let (ds, src') = span isSpace src
                    in [((), src')])

pFail :: Parser a
pFail = P (\src -> [])

satisfy :: Parser a -> (a -> Bool) -> Parser a
satisfy p cond = do x <- p
                    if cond x
                        then return x
                        else pFail

char :: Char -> Parser Char
char x = satisfy item (== x)

string :: String -> Parser String
string [] = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)





int :: Parser AST
int =  LitInteger . toInteger <$> natural

addExp :: Parser AST
addExp = do m <- mexp
            string "+"
            e <- expr
            return (BinOp Addition m e)




subExp :: Parser AST
subExp = subExp' id

subExp' :: (AST -> AST) -> Parser AST
subExp' f = do x <- mexp
               string "-"
               subExp' (\y -> BinOp Subtraction (f x) y)
            <|>
            do x <- addExp <|> mexp -- This seems odd, but using expr causes infinite recursion??
               return (f x)

divExp :: Parser AST
divExp = divExp' id

divExp' :: (AST -> AST) -> Parser AST
divExp' f = do x <- term
               string "/"
               divExp' (\y -> BinOp Division (f x) y)
            <|>
            do x <- mulExpr <|> term
               return (f x)

expr :: Parser AST
expr = addExp <|>
       subExp <|>
       mexp

mulExpr :: Parser AST
mulExpr = do t <- term
             string "*"
             m <- mexp
             return (BinOp Multiplication t m)

mexp :: Parser AST
mexp = mulExpr <|>
       divExp <|>
       term

term :: Parser AST
term = do pSpace
          x <- int
          pSpace
          return x

       <|>
       
       do pSpace
          string "-"
          pSpace
          t <- term
          return (UnOp Negation t)

       <|>

       do pSpace
          string "("
          e <- expr
          string ")"
          pSpace
          return e
