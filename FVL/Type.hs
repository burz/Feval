module FVL.Type
( typecheck
) where

import Prelude hiding (lookup)
import qualified Data.Set as Set
import Control.Monad.State
import Control.Applicative

import FVL.Algebra
import FVL.TypeAST

type Equation = (FType, FType)
type Equations = Set.Set Equation
data Updates = Updates Equations Bool

(=>>) :: Updates -> (Equations -> Updates) -> Updates
(=>>) (Updates e True) f = let (Updates e' _) = f e in Updates e' True
(=>>) (Updates e _) f = f e

addEquation' :: Equation -> Equations -> Updates
addEquation' eq e = if Set.member eq e then Updates e False else Updates (Set.insert eq e) True

addEquation :: Equation -> Equations -> Updates
addEquation (x, y) e = addEquation' (x, y) e =>> addEquation' (y, x)

addTransitives :: Equations -> Updates
addTransitives e = Set.fold check_element (Updates e False) e
    where check_element eq u = u =>> Set.fold (run_through eq) u
          run_through (x, y) (x', y') u = if x' == y
              then u =>> addEquation (x, y')
              else u

addArrowsAndLists :: Equations -> Updates
addArrowsAndLists e = Set.fold check_element (Updates e False) e
    where check_element (FArrow x y, FArrow x' y') u
              = u =>> addEquation (x, x') =>> addEquation (y, y')
          check_element (FList x, FList y) u
              = u =>> addEquation (x, y) =>> addEquation (y, x)
          check_element _ u = u

close :: Equations -> Equations
close e = let Updates e' r = addTransitives e =>> addArrowsAndLists in if r
    then close e' 
    else e'

inconsistent :: Equations -> Bool
inconsistent e = Set.fold check False e
    where check (FInt, FBool) _ = True
          check (FInt, FArrow _ _) _ = True
          check (FInt, FList _) _ = True
          check (FBool, FArrow _ _) _ = True
          check (FBool, FList _) _ = True
          check (FArrow _ _, FList _) _ = True
          check (FNotClosed, _) _ = True
          check _ r = r

choose :: Int -> Equations -> Equation -> FType -> FType
choose _ _ _ FInt = FInt
choose _ _ _ FBool = FBool
choose n e (FVar n', FArrow x y) (FVar n'') = if n' == n
    then FArrow (substitute x e) (substitute y e)
    else FVar n''
choose n e (FVar n', FList t) r = if n == n'
    then FList (substitute t e)
    else r
choose n _ (FVar n', y) (FVar n'') = if n /= n' then FVar n'' else case y of
    FInt -> FInt
    FBool -> FBool
    FVar n' -> if n' < n
        then if n'' < n' then FVar n'' else FVar n'
        else if n'' < n then FVar n'' else FVar n
choose _ _ _ r = r

substitute :: FType -> Equations -> FType
substitute FInt _ = FInt
substitute FBool _ = FBool
substitute (FVar n) e = Set.fold (choose n e) (FVar n) e
substitute (FArrow x y) e = FArrow (substitute x e) (substitute y e)
substitute (FList t) e = FList $ substitute t e

add :: Equation -> Equations -> Equations
add (x, y) e = Set.insert (x, y) (Set.insert (y, x) e)

twoAdd :: Equation -> Equation -> Equations -> Equations -> Equations
twoAdd eq eq' e e' = add eq $ add eq' (Set.union e e')

threeAdd :: Equation -> Equation -> Equation -> Equations -> Equations -> Equations -> Equations
threeAdd eq eq' eq'' e e' e'' = add eq $ Set.union e (twoAdd eq' eq'' e' e'')

type Counter = State Int

doNothing :: Counter Int
doNothing = state (\i -> (i, i))

newHandle :: Counter Int
newHandle = state (\i -> (i, i + 1))

type Hypotheses = [(String, FType)]

lookup :: String -> Hypotheses -> Maybe FType
lookup s [] = Nothing
lookup s ((s', t):xs) = if s' == s then Just t else lookup s xs

type TypeResult = (FType, Equations)
type TypeMAlgebra = MAlgebra Counter (Expr (LazyFix Expr)) TypeResult

alg :: Hypotheses -> TypeMAlgebra
alg _ (CInt _) = (\_ -> (FInt, Set.empty)) <$> doNothing
alg _ (CBool _) = (\_ -> (FBool, Set.empty)) <$> doNothing
alg g (CVar s) = (\_ -> let r = lookup s g in case r of
    Nothing -> (FNotClosed, Set.insert (FNotClosed, FNotClosed) Set.empty)
    Just t -> (t, Set.empty)) <$> doNothing
alg _ (x `Add` y) = (\(t, e) (t', e') -> (FInt, twoAdd (t, FInt) (t', FInt) e e')) <$> x <*> y
alg _ (x `Sub` y) = (\(t, e) (t', e') -> (FInt, twoAdd (t, FInt) (t', FInt) e e')) <$> x <*> y
alg _ (x `Mul` y) = (\(t, e) (t', e') -> (FInt, twoAdd (t, FInt) (t', FInt) e e')) <$> x <*> y
alg _ (x `Div` y) = (\(t, e) (t', e') -> (FInt, twoAdd (t, FInt) (t', FInt) e e')) <$> x <*> y
alg _ (x `And` y) = (\(t, e) (t', e') -> (FBool, twoAdd (t, FBool) (t', FBool) e e')) <$> x <*> y
alg _ (x `Or` y) = (\(t, e) (t', e') -> (FBool, twoAdd (t, FBool) (t', FBool) e e')) <$> x <*> y
alg _ (x `Equal` y) = (\(t, e) (t', e') -> (FBool, twoAdd (t, FInt) (t', FInt) e e')) <$> x <*> y
alg _ (x `Less` y) = (\(t, e) (t', e') -> (FBool, twoAdd (t, FInt) (t', FInt) e e')) <$> x <*> y
alg _ Empty = (\n -> let h = FVar n in (FList h, Set.empty)) <$> newHandle
alg _ (x `Cons` y) = (\n (t, e) (t', e') -> let h = FVar n in
    (FList h, twoAdd (t, h) (t', FList h) e e')) <$> newHandle <*> x <*> y
alg _ (Not x) = (\(t, e) -> (FBool, add (t, FBool) e)) <$> x
alg _ (If p x y) = (\n (t, e) (t', e') (t'', e'') -> let h = FVar n in
    (h, threeAdd (t, FBool) (t', t'') (t'', h) e e' e'')) <$> newHandle <*> p <*> x <*> y
alg g (Function x p) = newHandle >>= \n -> let h = FVar n in
    typecheck' ((x, h) : g) p >>= \(t, e) -> return (FArrow h t, e)
alg _ (Appl f x) = (\n (t, e) (t', e') -> let h = FVar n in
    (h, add (t, FArrow t' h) (Set.union e e'))) <$> newHandle <*> f <*> x
alg g (LetRec f x p r) = newHandle >>= \n -> newHandle >>= \n' ->
    let h = FVar n in let h' = FVar n' in
    typecheck' ((f, h) : (x, h') : g) p >>= \(t, e) ->
    typecheck' ((f, h) : g) r >>= \(t', e') ->
    return (t', add (h, FArrow h' t) (Set.union e e'))
alg g (Case p x s s' y) = newHandle >>= \n -> let h = FVar n in p >>= \(t, e) ->
    x >>= \(t', e') -> typecheck' ((s, h) : (s', FList h) : g) y >>= \(t'', e'') ->
    return (t', twoAdd (t, FList h) (t', t'') e (Set.union e' e''))

typecheck' :: Hypotheses -> LazyFix Expr -> Counter TypeResult
typecheck' g e = lazyMCata (alg g) e

typecheck :: LazyFix Expr -> Maybe FType
typecheck e = let (t, e') = evalState (typecheck' [] e) 0
    in let e'' = close e'
    in if inconsistent e'' then Nothing else Just (substitute t e'')

