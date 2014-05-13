type Algebra f a = f a -> a
newtype Fix f = Fx (f (Fix f))

unFix :: Fix f -> f (Fix f)
unFix (Fx x) = x

cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg = alg . fmap (cata alg) . unFix

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

type BasicAlg = Algebra ExprF CVal

alg :: BasicAlg
alg (Const v) = v
alg ((CInt x) `Add` (CInt y)) = CInt $ x + y
alg ((CInt x) `Mul` (CInt y)) = CInt $ x * y
alg ((CBool x) `And` (CBool y)) = CBool $ x && y
alg ((CBool x) `Or` (CBool y)) = CBool $ x || y
alg _ = MismatchedTypes

eval :: Fix ExprF -> CVal
eval = cata alg

-- Examples:

intExpr = Fx $ (Fx $ (Fx $ Const (CInt 2)) `Add` 
               (Fx $ Const (CInt 3))) `Mul` (Fx $ Const (CInt 4))

boolExpr = Fx $ (Fx $ (Fx $ Const (CBool False)) `Or`
                (Fx $ Const (CBool True))) `And` (Fx $ Const (CBool True))

badExpr = Fx $ (Fx $ Const (CBool False)) `Or` (Fx $ Const (CInt 500))


main = mapM_ print [ eval intExpr
                   , eval boolExpr
                   , eval badExpr
                   ]

