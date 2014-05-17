module EFeval
( Expr(..)
, run
) where

import qualified AST as AST
import Algebra
import qualified Feval as F

data Expr a
    = CInt Int
    | CBool Bool
    | CVar String
    | Add a a 
    | Mul a a 
    | And a a 
    | Or a a 
    | Equal a a 
    | If a a a 
    | Function String a
    | Appl a a
    | Let String a a
    | Semi a a

instance Functor Expr where
    fmap eval (CInt n) = CInt n
    fmap eval (CBool b) = CBool b
    fmap eval (CVar s) = CVar s
    fmap eval (x `Add` y) = eval x `Add` eval y
    fmap eval (x `Mul` y) = eval x `Mul` eval y
    fmap eval (x `And` y) = eval x `And` eval y
    fmap eval (x `Or` y) = eval x `Or` eval y
    fmap eval (x `Equal` y) = eval x `Equal` eval y
    fmap eval (If p e1 e2) = If (eval p) (eval e1) (eval e2) 
    fmap eval (Function s p) = Function s (eval p)
    fmap eval (Appl f x) = Appl (eval f) (eval x)
    fmap eval (Let s x y) = Let s (eval x) (eval y)
    fmap eval (Semi x y) = Semi (eval x) (eval y)

alg :: Algebra Expr (Fix AST.ExprF)
alg (CInt n) = Fx $ AST.CInt n
alg (CBool b) = Fx $ AST.CBool b
alg (CVar s) = Fx $ AST.CVar s
alg (Add x y) = Fx $ AST.Add x y
alg (Mul x y) = Fx $ AST.Mul x y
alg (And x y) = Fx $ AST.And x y
alg (Or x y) = Fx $ AST.Or x y
alg (Equal x y) = Fx $ AST.Equal x y
alg (If p x y) = Fx $ AST.If p x y
alg (Function s p) = Fx $ AST.Function s p
alg (Appl f x) = Fx $ AST.Appl f x
alg (Let s x y) = Fx $ AST.Appl (Fx $ AST.Function s y) x
alg (Semi x y) = Fx $ AST.Appl (Fx $ AST.Function "_" y) x

translate :: Fix Expr -> Fix AST.ExprF
translate = cata alg

run :: Fix Expr -> F.Result
run = F.run . translate

