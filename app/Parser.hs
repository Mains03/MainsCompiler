module Parser where

import Text.ParserCombinators.Parsec
import Text.Pretty.Simple
import Control.Monad

data Stm
    = Assignment Aexp Aexp
    | Skip
    | If Bexp Stm Stm
    | While Bexp Stm
        deriving Show

data Aexp
    = Num Int
    | Var String
    | Add Aexp Aexp
    | Mul Aexp Aexp
    | Sub Aexp Aexp
    | ParenthAexp Aexp
        deriving Show

data Bexp
    = Bexp  Bool
    | Equal Aexp Aexp
    | LE    Aexp Aexp
    | Not   Bexp
    | And   Bexp Bexp
    | ParenthBexp Bexp
        deriving Show

parseProgram = parse (sepEndBy stm (char ';') <* eof) ""

stm =   try assignment
    <|> try skip
    <|> try ifExpr
    <|> try while
    <?> "statement"

assignment = do
    x <- variable
    spaces >> string ":="
    Assignment x <$> aexpr

skip = spaces >> string "skip" >> return Skip

ifExpr = do
    spaces >> string "if"
    b <- bexpr
    spaces >> string "then"
    s <- stm
    spaces >> string "else"
    If b s <$> stm

while = do
    spaces >> string "while"
    b <- bexpr
    spaces >> string "do"
    While b <$> stm



aexpr =   try addition
      <|> try aterm
      <?> "aexpr"

aterm =   try multiplication
      <|> try afactor
      <?> "aterm"

afactor =   try number
        <|> try variable
        <|> try parenthAexpr
        <?> "afactor"

addition = do
    a1 <- aterm
    spaces >> char '+' >> spaces
    Add a1 <$> aexpr

multiplication = do
    a1 <- afactor
    spaces >> char '*' >> spaces
    Mul a1 <$> aterm

number = spaces >> Num . read <$> many1 digit

variable = spaces >> Var <$> many1 letter

parenthAexpr = do
    spaces >> char '('
    a <- aexpr
    spaces >> char ')'
    return $ ParenthAexp a



bexpr =   try equalBexpr
      <|> try leBexpr
      <|> try andBexpr
      <|> try bterm
      <?> "bexpr"

bterm =   try trueLit
      <|> try falseLit
      <|> try notBexpr
      <|> try parenthesisedBexpr
      <?> "bterm"

equalBexpr = do
    a <- aexpr
    spaces >> string "="
    Equal a <$> aexpr

leBexpr = do
    a <- aexpr
    spaces >> string "<="
    LE a <$> aexpr

andBexpr = do
    b <- bterm
    spaces >> char '.'
    And b <$> bexpr

trueLit = spaces >> string "true" >> return (Bexp True)

falseLit = spaces >> string "false" >> return (Bexp False)

notBexpr = spaces >> char '¬' >> Not <$> bexpr

parenthesisedBexpr = do
    spaces >> char '('
    b <- bexpr
    spaces >> char ')'
    return $ ParenthBexp b
