module Algebra
( Algebra
, MAlgebra
, Fix(..)
, unFix
, cata
, mcata
) where

type Algebra f a = f a -> a
type MAlgebra m f a = f (m a) -> m a

newtype Fix f = Fx (f (Fix f))

unFix :: Fix f -> f (Fix f)
unFix (Fx x) = x

cata :: Functor f => Algebra f a -> Fix f -> a
cata alg = alg . fmap (cata alg) . unFix

mcata :: Functor f => MAlgebra m f a -> Fix f -> m a
mcata alg = alg . fmap (cata alg) . unFix

