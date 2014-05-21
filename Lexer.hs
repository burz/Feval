module Lexer
( opNames
, identifier
, reserved
, reservedOp
, parens
, comma
, integer
, whiteSpace
) where

import Text.Parsec
import qualified Text.Parsec.Token as Token
import Text.Parsec.Language

names = words "True False Function If Then Else Let In Case Of"
opNames = words "-> && || ! + - * / % = ; < <= > >= : [ ]"

lexer = Token.makeTokenParser emptyDef
    { Token.commentStart = "/*"
    , Token.commentEnd = "*/"
    , Token.commentLine = "#"
    , Token.identStart = letter
    , Token.identLetter = alphaNum <|> char '_' <|> char '\''
    , Token.reservedNames = names
    , Token.reservedOpNames = opNames
    }

identifier = Token.identifier lexer
reserved = Token.reserved lexer
reservedOp = Token.reservedOp lexer
parens = Token.parens lexer
comma = Token.comma lexer
integer = Token.integer lexer
whiteSpace = Token.whiteSpace lexer

