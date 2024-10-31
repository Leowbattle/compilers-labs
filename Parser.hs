module Parser where

import Control.Applicative (Alternative, empty, (<|>))
import Data.Char

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

spaces :: Parser ()
spaces = P (\src -> let (ds, src') = span isSpace src
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

token :: Parser a -> Parser a
token p = do
    spaces
    v <- p
    spaces
    return v
