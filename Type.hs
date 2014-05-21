module Type
( typecheck
) where

import Prelude hiding (lookup)
import qualified Data.Set as Set
import Control.Monad.State
import Control.Applicative

import Algebra
import TypeAST

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

add_arrows :: Equations -> Updates
add_arrows e = Set.fold check_element (Updates e False) e
    where check_element (FArrow x y, FArrow x' y') u
              = u =>> add_equation (x, x') =>> add_equation (y, y')
          check_element _ u = u

close :: Equations -> Equations
close e = let Updates e' r = add_transitives e =>> add_arrows in if r
    then close e' 
    else e'

inconsistent :: Equations -> Bool
inconsistent e = Set.fold check False e
    where check (FInt, FBool) _ = True
          check (FInt, FArrow _ _) _ = True
          check (FBool, FArrow _ _) _ = True
          check (FNotClosed, _) _ = True
          check _ r = r

choose :: Int -> Equations -> Equation -> FType -> FType
choose _ _ _ FInt = FInt
choose _ _ _ FBool = FBool
choose n e (FVar n', FArrow x y) (FVar n'') = if n' == n
    then FArrow (substitute x e) (substitute y e)
    else FVar n''
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

add :: Equation -> Equations -> Equations
add (x, y) e = Set.insert (x, y) (Set.insert (y, x) e)

two_add :: Equation -> Equation -> Equations -> Equations -> Equations
two_add eq eq' e e' = add eq $ add eq' (Set.union e e')

three_add :: Equation -> Equation -> Equation -> Equations -> Equations -> Equations -> Equations
three_add eq eq' eq'' e e' e'' = add eq $ Set.union e (two_add eq' eq'' e' e'')

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
alg _ (x `Add` y) = (\(t, e) (t', e') -> (FInt, two_add (t, FInt) (t', FInt) e e')) <$> x <*> y
alg _ (x `Sub` y) = (\(t, e) (t', e') -> (FInt, two_add (t, FInt) (t', FInt) e e')) <$> x <*> y
alg _ (x `Mul` y) = (\(t, e) (t', e') -> (FInt, two_add (t, FInt) (t', FInt) e e')) <$> x <*> y
alg _ (x `Div` y) = (\(t, e) (t', e') -> (FInt, two_add (t, FInt) (t', FInt) e e')) <$> x <*> y
alg _ (x `And` y) = (\(t, e) (t', e') -> (FBool, two_add (t, FBool) (t', FBool) e e')) <$> x <*> y
alg _ (x `Or` y) = (\(t, e) (t', e') -> (FBool, two_add (t, FBool) (t', FBool) e e')) <$> x <*> y
alg _ (x `Equal` y) = (\(t, e) (t', e') -> (FBool, two_add (t, FInt) (t', FInt) e e')) <$> x <*> y
alg _ (x `Less` y) = (\(t, e) (t', e') -> (FBool, two_add (t, FInt) (t', FInt) e e')) <$> x <*> y
alg _ (Not x) = (\(t, e) -> (FBool, add (t, FBool) e)) <$> x
alg _ (If p x y) = (\n (t, e) (t', e') (t'', e'') -> let h = FVar n in
    (h, three_add (t, FBool) (t', t'') (t'', h) e e' e'')) <$> newHandle <*> p <*> x <*> y
alg g (Function x p) = newHandle >>= \n -> let h = FVar n in
    typecheck' ((x, h) : g) p >>= \(t, e) -> return (FArrow h t, e)
alg _ (Appl f x) = (\n (t, e) (t', e') -> let h = FVar n in
    (h, add (t, FArrow t' h) (Set.union e e'))) <$> newHandle <*> f <*> x
alg g (LetRec f x p r) = newHandle >>= \n -> newHandle >>= \n' ->
    let h = FVar n in let h' = FVar n' in
    typecheck' ((f, h) : (x, h') : g) p >>= \(t, e) ->
    typecheck' ((f, h) : g) r >>= \(t', e') ->
    return (t', add (h, FArrow h' t) (Set.union e e'))

typecheck' :: Hypotheses -> LazyFix Expr -> Counter TypeResult
typecheck' g e = lazyMCata (alg g) e

typecheck :: LazyFix Expr -> Maybe FType
typecheck e = let (t, e') = evalState (typecheck' [] e) 0
    in let e'' = close e'
    in if inconsistent e'' then Nothing else Just (substitute t e'')

