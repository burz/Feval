{-# LANGUAGE FlexibleInstances #-}

module Algebra
( Algebra
, MAlgebra
, Fix(..)
, LazyFix(..)
, cata
, lazyCata
, mcata
) where

type Algebra f a = f a -> a
type MAlgebra m f a = f (m a) -> m a

newtype Fix f = Fx (f (Fix f))
newtype LazyFix f = Fx' (f (LazyFix f) (LazyFix f))

unFix :: Fix f -> f (Fix f)
unFix (Fx x) = x

lazyUnFix :: LazyFix f -> f (LazyFix f) (LazyFix f)
lazyUnFix (Fx' x) = x

cata :: Functor f => Algebra f a -> Fix f -> a
cata alg = alg . fmap (cata alg) . unFix

lazyCata :: Functor (f (LazyFix f)) => Algebra (f (LazyFix f)) b -> LazyFix f -> b
lazyCata alg = alg . fmap (lazyCata alg) . lazyUnFix

mcata :: Functor f => MAlgebra m f a -> Fix f -> m a
mcata alg = alg . fmap (cata alg) . unFix

