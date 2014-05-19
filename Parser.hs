module Parser
(
) where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Control.Monad
import Control.Applicative ((<$>), (<$), (<*>), (<*), (*>))

import Algebra
import EFeval
import Lexer

type ExprParser = Parser (Fix Expr)

cint :: ExprParser
cint = Fx . CInt <$> integer

cbool :: Parser (Fix Expr)
cbool = Fx (CBool True) <$ reserved "True"
    <|> Fx (CBool False) <$ reserved "False"

cvar :: ExprParser
cvar = Fx . CVar <$> identifier

prefix n f = Prefix (reservedOp n *> return (Fx . f))
binary n f a = Infix (reservedOp n *> return (\x -> Fx . f x)) a

opTable = [ [ prefix "!" Not ]
          , [ binary "&&" And AssocLeft
            , binary "*" Mul AssocLeft
            , binary "/" Div AssocLeft
            ]
          , [ binary "||" Or AssocLeft
            , binary "+" Add AssocLeft
            , binary "-" Sub AssocLeft
            ]
          , [ binary "=" Equal AssocLeft ]
          , [ binary ";" Semi AssocLeft ]
          ]

opExpr :: ExprParser
opExpr = buildExpressionParser opTable term

ifExpr :: ExprParser
ifExpr = reserved "If" *> ((\x y -> Fx . If x y)
    <$> expr <*> (reserved "Then" *> expr) <*> (reserved "Else" *> expr))

function :: ExprParser
function = reserved "Function" *> ((\x -> Fx . Function x)
    <$> identifier <*> (reservedOp "->" *> expr))

-- some sort of error with appl ==> consumes operators
appl :: ExprParser
appl = try $ do
    s <- sepBy1 factor whiteSpace
    case s of
        (x:y:xs) -> return $ foldr (\a b -> Fx $ Appl b a) (Fx $ Appl x y) xs
        (x:[]) -> mzero

letExpr :: ExprParser
letExpr = reserved "Let" *> do
    s <- sepBy1 identifier whiteSpace
    reservedOp "="
    e <- expr
    reserved "In"
    e' <- expr
    case s of (x:xs) -> return . Fx $ Let x xs e e'

factor :: ExprParser
factor =  cint
    <|> cbool
    <|> cvar
    <|> parens expr

term :: ExprParser
term = appl
    <|> factor

expr :: ExprParser
expr =  function
    <|> opExpr
    <|> letExpr
    <|> term

parseString :: String -> Either ParseError (Fix Expr)
parseString s = parse (expr <* eof) "" s

parseFile :: FilePath -> IO (Either ParseError (Fix Expr))
parseFile f = parseFromFile (expr <* eof) f

