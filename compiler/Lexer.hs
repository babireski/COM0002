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
    T.reservedOpNames = ["+", "-", "/", "*", "==", "/=", "<", ">", "<=", ">=", "&&", "||", "!"],
    T.reservedNames   = ["int", "double", "string"]
  }

lexer = T.makeTokenParser definition

number      = T.naturalOrFloat lexer
symbol      = T.symbol         lexer
parentheses = T.parens         lexer
operator    = T.reservedOp     lexer
literal     = T.stringLiteral  lexer
identifier  = T.identifier     lexer
comma       = T.comma          lexer
reserved    = T.reserved       lexer

binary name fun = Infix  (do {operator name; return fun})
prefix name fun = Prefix (do {operator name; return fun})

table =
  [
    [prefix "-" Neg],
    [binary "*" (:*:) AssocLeft, binary "/" (:/:) AssocLeft],
    [binary "+" (:+:) AssocLeft, binary "-" (:-:) AssocLeft]
  ]

ltable =
  [
    [prefix "!" Not],
    [binary "&&" (:&:) AssocLeft, binary "||" (:|:) AssocLeft]
  ]