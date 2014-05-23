module FVL.EF
( F.Result(..)
, Expr(..)
, showTranslation
, run
, ParseError
, parseRun
, parseFileRun
) where

import FVL.Algebra
import qualified FVL.FAST as FAST
import qualified FVL.F as F
import FVL.EFAST
import FVL.Parser

argument :: String -> [String] -> Bool
argument s [] = False
argument s (x:xs) = if x == s then True else argument s xs

recursiveAlg :: String -> Algebra FAST.Expr Bool
recursiveAlg _ (FAST.CInt n) = False
recursiveAlg _ (FAST.CBool b) = False
recursiveAlg s (FAST.CVar s') = if s' == s then True else False
recursiveAlg _ (FAST.Add x y) = x || y
recursiveAlg _ (FAST.Sub x y) = x || y
recursiveAlg _ (FAST.Mul x y) = x || y
recursiveAlg _ (FAST.Div x y) = x || y
recursiveAlg _ (FAST.And x y) = x || y
recursiveAlg _ (FAST.Or x y) = x || y
recursiveAlg _ (FAST.Not x) = x
recursiveAlg _ (FAST.Equal x y) = x || y
recursiveAlg _ (FAST.Less x y) = x || y
recursiveAlg _ FAST.Empty = False
recursiveAlg _ (FAST.Cons x y) = x || y
recursiveAlg _ (FAST.If p x y) = p || x || y
recursiveAlg s (FAST.Function s' p) = if s' == s then False else p
recursiveAlg _ (FAST.Appl f x) = f || x
recursiveAlg s (FAST.LetRec f x p e) = if f == s
    then False
    else if x == s then p else p || e
recursiveAlg _ (FAST.Case p x _ _ y) = p || x || y

recursive :: String -> Fix FAST.Expr -> Bool
recursive s = cata $ recursiveAlg s

createWrapper :: [String] -> Fix FAST.Expr -> Fix FAST.Expr
createWrapper [] e = e
createWrapper (x:xs) e = Fx $ FAST.Function x (createWrapper xs e)

letTransform :: String -> Fix FAST.Expr -> Fix FAST.Expr -> Fix FAST.Expr
letTransform s x y = Fx $ FAST.Appl (Fx $ FAST.Function s y) x

modTransform :: Fix FAST.Expr -> Fix FAST.Expr -> Fix FAST.Expr
modTransform x y = let y' = Fx $ FAST.Mul y (Fx $ FAST.Div x y)
    in Fx $ FAST.Sub x y'

lteTransform :: Fix FAST.Expr -> Fix FAST.Expr -> Fix FAST.Expr
lteTransform x y = Fx $ FAST.Or (Fx $ FAST.Less x y) (Fx $ FAST.Equal x y)

gtTransform :: Fix FAST.Expr -> Fix FAST.Expr -> Fix FAST.Expr
gtTransform x y = Fx . FAST.Not $ lteTransform x y

gteTransform :: Fix FAST.Expr -> Fix FAST.Expr -> Fix FAST.Expr
gteTransform x y = Fx . FAST.Not . Fx $ FAST.Less x y

alg :: Algebra Expr (Fix FAST.Expr)
alg (CInt n) = Fx $ FAST.CInt n
alg (CBool b) = Fx $ FAST.CBool b
alg (CVar s) = Fx $ FAST.CVar s
alg (Add x y) = Fx $ FAST.Add x y
alg (Sub x y) = Fx $ FAST.Sub x y
alg (Mul x y) = Fx $ FAST.Mul x y
alg (Div x y) = Fx $ FAST.Div x y
alg (Mod x y) = modTransform x y
alg (And x y) = Fx $ FAST.And x y
alg (Or x y) = Fx $ FAST.Or x y
alg (Not x) = Fx $ FAST.Not x
alg (Equal x y) = Fx $ FAST.Equal x y
alg (Less x y) = Fx $ FAST.Less x y
alg (LessEq x y) = lteTransform x y
alg (Great x y) = gtTransform x y
alg (GreatEq x y) = gteTransform x y
alg Empty = Fx FAST.Empty
alg (Cons x y) = Fx $ FAST.Cons x y
alg (If p x y) = Fx $ FAST.If p x y
alg (Function s p) = Fx $ FAST.Function s p
alg (Appl f x) = Fx $ FAST.Appl f x
alg (Let f [] p e) = letTransform f p e
alg (Let f (a:as) p e) = if recursive f p
    then Fx $ FAST.LetRec f a (createWrapper as p) e
    else letTransform f (createWrapper (a:as) p) e
alg (Semi x y) = Fx $ FAST.Appl (Fx $ FAST.Function "_" y) x
alg (Case p x s t y) = Fx $ FAST.Case p x s t y

translate :: Fix Expr -> Fix FAST.Expr
translate = cata alg

showTranslation :: Fix Expr -> String
showTranslation = show . translate

run :: Fix Expr -> F.Result
run = F.run . translate

parseRun :: String -> Either ParseError F.Result
parseRun s = case parseString s of
    Left e -> Left e
    Right e -> Right $ run e

parseFileRun :: FilePath -> IO (Either ParseError F.Result)
parseFileRun p = do
    r <- parseFile p
    case r of
        Left e -> return $ Left e
        Right e -> return . Right $ run e

