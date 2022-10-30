module Parser where

import Text.ParserCombinators.Parsec
import Text.Pretty.Simple
import Control.Monad

data Aexp
    = Num Int
    | Var String
    | Add Aexp Aexp
    | Mul Aexp Aexp
    | Sub Aexp Aexp
        deriving Show

data Stm
    = Assignment Aexp Aexp
    | Skip
        deriving Show

tokenize :: [Char] -> Either ParseError [Stm]
tokenize = parse program ""

program = sepEndBy stm (char ';') <* eof

stm =   try assignment
    <|> try skip
    <?> "statement"

assignment = 
    do x <- variable
       spaces >> string ":=" >> spaces
       Assignment x <$> expr

skip = spaces >> string "skip" >> return Skip



expr =   try addition
     <|> try factor
     <?> "expr"

term =   try multiplication
     <|> try factor
     <?> "term"

factor =   number
       <|> variable
       <?> "factor"

addition =
    do a1 <- term
       spaces >> char '+' >> spaces
       Add a1 <$> expr

multiplication =
    do a1 <- factor
       spaces >> char '*' >> spaces
       Mul a1 <$> term



number = spaces >> Num . read <$> many1 digit

variable = spaces >> Var <$> many1 letter
