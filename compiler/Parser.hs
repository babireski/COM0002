module Parser where

import Text.Parsec
import Text.Parsec.Expr
import Lexer
import Types

-- Useful generic list parser
-- Maybe I should change this to commaSep
list element = sepBy element comma

-- Arithmetic expressions
constant = do {c <- number; case c of Left  n -> return (Con (Intcons n)); Right n -> return (Con (Doublecons n))}
factor = do {parentheses expression <|> constant <|> Lit <$> literal <|> try (do {i <- identifier; e <- parentheses (list expression); return (Cal i e)}) <|> Var <$> identifier <?> "simple expression"}
expression = buildExpressionParser table factor <?> "expression"

-- Relational expressions
relational = do {(operator "==" >> return (:==:)) <|> (operator ">=" >> return (:>=:)) <|> (operator "<=" >> return (:<=:)) <|> (operator ">" >> return (:>:)) <|> (operator "<" >> return (:<:)) <|> (operator "/=" >> return (:/=:)) <?> "relational operator"}
relation = do {e <- expression; r <- relational; r e <$> expression;}

-- Logical expressions
lfactor = do {parentheses logic <|> Rel <$> relation}
logic = buildExpressionParser ltable lfactor <?> "logical expression"

-- Parameter
vartype = do {(reserved "int" >> return Inttype) <|> (reserved "double" >> return Inttype) <|> (reserved "string" >> return Stringtype) <?> "type"}
parameter = do {t <- vartype; i <- identifier; return (i :#: t)}
parameters = list parameter

-- Function
rettype = do {vartype <|> (reserved "void" >> return Voidtype) <?> "type"}
function = do {t <- rettype; i <- identifier; p <- parentheses parameters; return (i :->: (p, t))}

-- Block
block = braces (many command)

-- Command
ifcommand = do {try (do {reserved "if"; l <- parentheses logic; b <- block; reserved "else"; If l b <$> block}) <|> do {reserved "if"; l <- parentheses logic; b <- block; return (If l b  [])}}
whilecommand = do {reserved "while"; l <- parentheses logic; While l <$> block;}
attribution = do {i <- identifier; operator "="; e <- expression; semicolon; return (Attribution i e)}
readcommand = do {reserved "read"; i <- parentheses identifier; semicolon; return (Read i)}
printcommand = do {reserved "print"; e <- parentheses expression; semicolon; return (Print e)}
command = do {ifcommand <|> whilecommand <|> attribution <|> readcommand <|> printcommand <?> "command"}

-- Run

run = do {e <- block; eof; return e}

parser string = case runParser run [] "Expressions" string of
  Left error -> print error
  Right x    -> print x

main =
  do
    e <- readFile "code.diq"
    parser e