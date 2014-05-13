module Algebra
( Algebra
, Fix(..)
, unFix
, cata
) where

type Algebra f a = f a -> a

newtype Fix f = Fx (f (Fix f)) 

unFix :: Fix f -> f (Fix f)
unFix (Fx x) = x 

cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg = alg . fmap (cata alg) . unFix

