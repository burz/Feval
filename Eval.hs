module Eval
( eval
) where

import AST
import Algebra

type EvalAlgebra = Algebra ExprF CVal

alg :: EvalAlgebra
alg (Const v) = v 
alg ((CInt x) `Add` (CInt y)) = CInt $ x + y 
alg ((CInt x) `Mul` (CInt y)) = CInt $ x * y 
alg ((CBool x) `And` (CBool y)) = CBool $ x && y
alg ((CBool x) `Or` (CBool y)) = CBool $ x || y
alg ((CInt x) `Equal` (CInt y)) = CBool $ x == y
alg _ = MismatchedTypes

eval :: Fix ExprF -> CVal
eval = cata alg

