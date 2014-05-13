module AST
( CVal(..)
, ExprF(..)
) where

data CVal = CInt Int | CBool Bool | MismatchedTypes deriving Show

data ExprF a
    = Const CVal
    | Add a a 
    | Mul a a 
    | And a a 
    | Or a a

instance Functor ExprF where
    fmap eval (Const i) = Const i
    fmap eval (left `Add` right) = (eval left) `Add` (eval right)
    fmap eval (left `Mul` right) = (eval left) `Mul` (eval right)
    fmap eval (left `And` right) = (eval left) `And` (eval right)
    fmap eval (left `Or` right) = (eval left) `Or` (eval right)

