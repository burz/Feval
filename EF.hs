module EF
( F.Result(..)
, Expr(..)
, run
, ParseError
, parseRun
, parseFileRun
) where

import Algebra
import qualified FAST as FAST
import qualified F as F
import EFAST
import Parser

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
recursiveAlg _ (FAST.If p x y) = p || x || y
recursiveAlg s (FAST.Function s' p) = if s' == s then False else p
recursiveAlg _ (FAST.Appl f x) = f || x
recursiveAlg s (FAST.LetRec f x p e) = if f == s
    then False
    else if x == s then p else p || e

recursive :: String -> Fix FAST.Expr -> Bool
recursive s = cata $ recursiveAlg s

createWrapper :: [String] -> Fix FAST.Expr -> Fix FAST.Expr
createWrapper [] e = e
createWrapper (x:xs) e = Fx $ FAST.Function x (createWrapper xs e)

letTransform :: String -> Fix FAST.Expr -> Fix FAST.Expr -> Fix FAST.Expr
letTransform s x y = Fx $ FAST.Appl (Fx $ FAST.Function s y) x

alg :: Algebra Expr (Fix FAST.Expr)
alg (CInt n) = Fx $ FAST.CInt n
alg (CBool b) = Fx $ FAST.CBool b
alg (CVar s) = Fx $ FAST.CVar s
alg (Add x y) = Fx $ FAST.Add x y
alg (Sub x y) = Fx $ FAST.Sub x y
alg (Mul x y) = Fx $ FAST.Mul x y
alg (Div x y) = Fx $ FAST.Div x y
alg (And x y) = Fx $ FAST.And x y
alg (Or x y) = Fx $ FAST.Or x y
alg (Not x) = Fx $ FAST.Not x
alg (Equal x y) = Fx $ FAST.Equal x y
alg (If p x y) = Fx $ FAST.If p x y
alg (Function s p) = Fx $ FAST.Function s p
alg (Appl f x) = Fx $ FAST.Appl f x
alg (Let f [] p e) = letTransform f p e
alg (Let f (a:as) p e) = if recursive f p
    then Fx $ FAST.LetRec f a (createWrapper as p) e
    else letTransform f (createWrapper (a:as) p) e
alg (Semi x y) = Fx $ FAST.Appl (Fx $ FAST.Function "_" y) x

translate :: Fix Expr -> Fix FAST.Expr
translate = cata alg

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

