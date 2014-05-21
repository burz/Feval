module Lexer
( reservedOpNames
, identifier
, reserved
, reservedOp
, parens
, integer
, whiteSpace
) where

import Text.Parsec
import qualified Text.Parsec.Token as Token
import Text.Parsec.Language

reservedOpNames = words "-> && || ! + - * / = ; <"

lexer = Token.makeTokenParser emptyDef
    { Token.commentStart = "/*"
    , Token.commentEnd = "*/"
    , Token.commentLine = "#"
    , Token.identStart = letter
    , Token.identLetter = alphaNum <|> char '_' <|> char '\''
    , Token.reservedNames = words "True False Function If Then Else Let In"
    , Token.reservedOpNames = reservedOpNames
    }

identifier = Token.identifier lexer
reserved = Token.reserved lexer
reservedOp = Token.reservedOp lexer
parens = Token.parens lexer
integer = Token.integer lexer
whiteSpace = Token.whiteSpace lexer

