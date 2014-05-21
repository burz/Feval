module Parser
( ParseError
, parseString
, parseFile
) where

import Text.Parsec hiding (Empty)
import Text.Parsec.String
import Text.Parsec.Expr
import Control.Monad
import Control.Applicative ((<$>), (<$), (<*>), (<*), (*>))

import Algebra
import EFAST
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
          , [ appl ]
          , [ binary "*" Mul AssocLeft
            , binary "/" Div AssocLeft
            , binary "%" Mod AssocLeft ]
          , [ binary "+" Add AssocLeft
            , binary "-" Sub AssocLeft
            ]
          , [ binary "=" Equal AssocLeft
            , binary "<" Less AssocLeft
            , binary "<=" LessEq AssocLeft
            , binary ">" Great AssocLeft
            , binary ">=" GreatEq AssocLeft
            ]
          , [ binary "&&" And AssocLeft ]
          , [ binary "||" Or AssocLeft ]
          , [ binary ":" Cons AssocRight ]
          , [ binary ";" Semi AssocLeft ]
          ]

opExpr :: ExprParser
opExpr = buildExpressionParser opTable term

list :: ExprParser
list = do
    reservedOp "["
    l <- sepBy expr comma
    reservedOp "]"
    return $ toCons l
    where toCons [] = Fx Empty
          toCons (x:xs) = Fx $ Cons x (toCons xs)

ifExpr :: ExprParser
ifExpr = reserved "If" *> ((\x y -> Fx . If x y)
    <$> expr <*> (reserved "Then" *> expr) <*> (reserved "Else" *> expr))

function :: ExprParser
function = reserved "Function" *> ((\x -> Fx . Function x)
    <$> identifier <*> (reservedOp "->" *> expr))

appl = Infix (whiteSpace *> notFollowedBy (choice $ map reservedOp opNames)
    *> return (\x y -> Fx $ Appl x y)) AssocLeft

letExpr :: ExprParser
letExpr = reserved "Let" *> do
    s <- sepBy1 identifier whiteSpace
    reservedOp "="
    e <- expr
    reserved "In"
    e' <- expr
    case s of (x:xs) -> return . Fx $ Let x xs e e'

caseExpr :: ExprParser
caseExpr = reserved "Case" *> do
    p <- expr
    reserved "Of" *> reservedOp "[" *> reservedOp "]" *> reservedOp "->"
    x <- expr
    reservedOp "|"
    (s, t) <- parens $ do{ s' <- identifier
                         ; reservedOp ":"
                         ; t' <- identifier
                         ; return (s', t')
                         }
    reservedOp "->"
    y <- expr
    return . Fx $ Case p x s t y

term :: ExprParser
term =  cint
    <|> cbool
    <|> cvar
    <|> list
    <|> parens expr

expr :: ExprParser
expr =  function
    <|> letExpr
    <|> ifExpr
    <|> caseExpr
    <|> opExpr
    <|> term

parseString :: String -> Either ParseError (Fix Expr)
parseString s = parse (expr <* eof) "" s

parseFile :: FilePath -> IO (Either ParseError (Fix Expr))
parseFile f = parseFromFile (expr <* eof) f

