module TAMParser where

import Data.Char
import Control.Applicative (Alternative, empty, many, (<|>))
import Parser
import CodeGenerator

arg0 :: String -> TAMInst -> Parser TAMInst
arg0 s i = token $ caseless s >> return i

arg1 :: String -> (a -> TAMInst) -> Parser a -> Parser TAMInst
arg1 s c p = do token $ caseless s
                x <- token p
                return $ c x

label :: Parser String
label = do cs <- many $ satisfy item (not . isSpace)
           return cs

instTAM :: Parser TAMInst
instTAM = arg1 "LOADL" LOADL natural <|>
          arg0 "ADD" ADD <|>
          arg0 "SUB" SUB <|>
          arg0 "MUL" MUL <|>
          arg0 "DIV" DIV <|>
          arg0 "NEG" NEG <|>
          arg0 "AND" AND <|>
          arg0 "OR" OR <|>
          arg0 "NOT" NOT <|>
          arg0 "LSS" LSS <|>
          arg0 "GRT" GRT <|>
          arg0 "EQL" EQL <|>
          arg0 "HALT" HALT <|>
          arg0 "GETINT" GETINT <|>
          arg0 "PUTINT" PUTINT <|>
          arg1 "LABEL" LABEL label <|>
          arg1 "JUMPIFZ" JUMPIFZ label <|>
          arg1 "JUMP" JUMP label <|>
          arg1 "LOAD" LOAD natural <|>
          arg1 "STORE" STORE natural

insts :: Parser [TAMInst]
insts = many instTAM

parseTAM :: String -> [TAMInst]
parseTAM src = let ((code,_):_) = parse insts src in code
