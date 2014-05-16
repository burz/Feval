module Type
( FType(..)
, typecheck
) where

import qualified Data.Set as Set
import Control.Monad.State
import Control.Applicative hiding (Const)

import AST
import Algebra

data FType = FInt | FBool | FVar Int deriving (Eq, Ord, Show)

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

choose :: Int -> Equation -> FType -> FType
choose n _ FInt = FInt
choose n _ FBool = FBool
choose n (FVar n', y) (FVar n'') = if n /= n' then FVar n'' else case y of
    FInt -> FInt
    FBool -> FBool
    FVar n' -> if n' < n
        then if n'' < n' then FVar n'' else FVar n'
        else if n'' < n then FVar n'' else FVar n
choose _ _ r = r

substitute :: FType -> Equations -> FType
substitute FInt _ = FInt
substitute FBool _ = FBool
substitute (FVar n) e = Set.fold (choose n) (FVar n) e

two_add :: Equation -> Equation -> Equations -> Equations -> Equations
two_add eq eq' e e' = Set.insert eq $ Set.insert eq' $ Set.union e e'

three_add :: Equation -> Equation -> Equation -> Equations -> Equations -> Equations -> Equations
three_add eq eq' eq'' e e' e'' = Set.insert eq $ Set.union e (two_add eq' eq'' e' e'')

type Counter = State Int

doNothing :: Counter Int
doNothing = state (\i -> (i, i))

newHandle :: Counter Int
newHandle = state (\i -> (i, i + 1))

type TypeResult = (FType, Equations)
type TypeMAlgebra = MAlgebra Counter ExprF TypeResult

alg :: TypeMAlgebra
alg (Const (CInt _)) = (\_ -> (FInt, Set.empty)) <$> doNothing
alg (Const (CBool _)) = (\_ -> (FBool, Set.empty)) <$> doNothing
alg (x `Add` y) = (\(t, e) (t', e') -> (FInt, two_add (t, FInt) (t', FInt) e e')) <$> x <*> y
alg (x `Mul` y) = (\(t, e) (t', e') -> (FInt, two_add (t, FInt) (t', FInt) e e')) <$> x <*> y
alg (x `And` y) = (\(t, e) (t', e') -> (FBool, two_add (t, FBool) (t', FBool) e e')) <$> x <*> y
alg (x `Or` y) = (\(t, e) (t', e') -> (FBool, two_add (t, FBool) (t', FBool) e e')) <$> x <*> y
alg (x `Equal` y) = (\(t, e) (t', e') -> (FBool, two_add (t, FInt) (t', FInt) e e')) <$> x <*> y
alg (If p x y) = (\n (t, e) (t', e') (t'', e'') ->
    let h = FVar n in
    (h, three_add (t, FBool) (t', t'') (t'', h) e e' e'')) <$> newHandle <*> p <*> x <*> y

typecheck :: Fix ExprF -> Maybe FType
typecheck e = let (t, e') = evalState (mcata alg e) 0
    in let e'' = close e'
    in if inconsistent e'' then Nothing else Just (substitute t e'')

