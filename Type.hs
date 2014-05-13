module Type
( FType(..)
, typecheck
) where

import qualified Data.Set as Set

import AST
import Algebra

data FType = FInt | FBool deriving (Eq, Ord, Show)

type Equation = (FType, FType)
type Equations = Set.Set Equation
data Updates = Updates Equations Bool

(=>>) :: Updates -> (Equations -> Updates) -> Updates
(=>>) (Updates e True) f = let (Updates e' _) = f e in Updates e' True
(=>>) (Updates e _) f = f e

add_equation' :: Equation -> Equations -> Updates
add_equation' eq e = if Set.member eq e then Updates e False else Updates (Set.insert eq e) True

add_equation :: Equation -> Equations -> Updates
add_equation (x, y) e = add_equation' (x, y) e =>> add_equation' (y, x)

add_transitives :: Equations -> Updates
add_transitives e = Set.fold check_element (Updates e False) e
    where check_element eq u = u =>> Set.fold (run_through eq) u
          run_through (x, y) (x', y') u = if x' == y
              then u =>> add_equation (x, y')
              else u

close :: Equations -> Equations
close e = let Updates e' r = add_transitives e in if r
    then close e' 
    else e'

inconsistent :: Equations -> Bool
inconsistent e = Set.fold check False e
    where check (FInt, FBool) _ = True
          check (FBool, FInt) _ = True
          check _ r = r

type TypeResult = (FType, Equations)
type TypeAlgebra = Algebra ExprF TypeResult

two_add :: Equation -> Equation -> Equations -> Equations -> Equations
two_add eq eq' e e' = Set.insert eq $ Set.insert eq' $ Set.union e e'

alg :: TypeAlgebra
alg (Const (CInt _)) = (FInt, Set.empty)
alg (Const (CBool _)) = (FBool, Set.empty)
alg ((t, e) `Add` (t', e')) = (FInt, two_add (t, FInt) (t', FInt) e e')
alg ((t, e) `Mul` (t', e')) = (FInt, two_add (t, FInt) (t', FInt) e e')
alg ((t, e) `And` (t', e')) = (FBool, two_add (t, FBool) (t', FBool) e e')
alg ((t, e) `Or` (t', e')) = (FBool, two_add (t, FBool) (t', FBool) e e')
alg ((t, e) `Equal` (t', e')) = (FBool, two_add (t, FInt) (t', FInt) e e')

typecheck :: Fix ExprF -> Maybe FType
typecheck e = let (t, e') = cata alg e
    in let e'' = close e'
    in if inconsistent e'' then Nothing else Just t

