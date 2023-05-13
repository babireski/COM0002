module Parser where

import Text.Parsec
import Text.Parsec.Expr
import Lexer
import Types

constant = do
{
  c <- number;
  case c of
    Left  n -> return (Con (Intcons n))
    Right n -> return (Con (Doublecons n))
}

factor = do
{
  parentheses expression <|> constant <?> "simple expression"
}


expression = buildExpressionParser table factor <?> "expression"

-- Run

run = do {e <- expression; eof; return e}

parser string = case runParser run [] "Expressoes" string of
  Left error -> print error
  Right x    -> print x

main =
  do
    e <- readFile "code.diq"
    parser e