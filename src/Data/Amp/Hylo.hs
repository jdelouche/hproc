{-# LANGUAGE DeriveFunctor #-}
module Data.Amp.Hylo (Fix(Fix), unFix, cata, ana) where
import Prelude ((.),Functor,fmap)
data Fix f = Fix (f (Fix f))
unFix :: Fix f -> f (Fix f)
unFix (Fix x) = x
cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg  = alg . fmap (cata alg) . unFix;
ana  :: Functor f => (a -> f a) -> a -> Fix f
ana coalg = Fix . fmap (ana coalg) . coalg
