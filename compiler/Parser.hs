module Parser where

import Text.Parsec
import Text.Parsec.Expr
import Lexer
import Types

list element = sepBy element comma

-- Mathematical expressions

constant = do {c <- number; case c of Left  n -> return (Con (Intcons n)); Right n -> return (Con (Doublecons n))}
factor = do {parentheses expression <|> constant <|> Lit <$> literal <|> try (do {i <- identifier; e <- parentheses (list expression); return (Cal i e)}) <|> Var <$> identifier <?> "simple expression"}
expression = buildExpressionParser table factor <?> "expression"

-- Relational expressions

relational =
  do operator "==" >> return (:==:)
    <|> do operator ">=" >> return (:>=:)
    <|> do operator "<=" >> return (:<=:)
    <|> do operator ">" >> return (:>:)
    <|> do operator "<" >> return (:<:)
    <|> do operator "/=" >> return (:/=:)
    <?> "relational operator"

relation = do {e <- expression; r <- relational; r e <$> expression;}

-- Logical expressions

lfactor = do {parentheses logic <|> Rel <$> relation}
logic = buildExpressionParser ltable lfactor <?> "logical expression"

-- Declaration

vartype = do {(reserved "int" >> return Inttype) <|> (reserved "double" >> return Inttype) <|> (reserved "string" >> return Stringtype) <?> "type"}
-- declaration = do {}

-- Run

run = do {e <- logic; eof; return e}

parser string = case runParser run [] "Expressions" string of
  Left error -> print error
  Right x    -> print x

main =
  do
    e <- readFile "code.diq"
    parser e