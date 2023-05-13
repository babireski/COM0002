module Lexer where

import Types
import Text.Parsec.Expr
import qualified Text.Parsec.Token as T
import Text.Parsec.Language


definition = emptyDef
  {
    T.commentStart    = "{-",
    T.commentEnd      = "-}",
    T.commentLine     = "--",
    T.reservedOpNames = ["+", "-", "/", "*"]
  }  

lexer = T.makeTokenParser definition

number = T.naturalOrFloat lexer
sym = T.symbol         lexer
parentheses = T.parens         lexer
opr = T.reservedOp     lexer

table =
  [
    [prefix "-" Neg],
    [binary "*" (:*:) AssocLeft, binary "/" (:/:) AssocLeft],
    [binary "+" (:+:) AssocLeft, binary "-" (:-:) AssocLeft]
  ]

binary name fun = Infix  (do {opr name; return fun})
prefix name fun = Prefix (do {opr name; return fun})