{-# LANGUAGE DeriveFunctor #-}

module Iteration where

newtype Mu f = In (f (Mu f))

data Mu' f = In' 

unIn :: Mu f -> f (Mu f)
unIn (In x) = x

data N x = Z | S x 
    deriving Functor

type Nat = Mu N

zeroN :: Nat
zeroN = In Z

succN :: Nat -> Nat
succN n = In (S n)

cata :: Functor f => (f c -> c) -> Mu f -> c
cata phi = phi . fmap (cata phi) . unIn

-- >>> tail []
-- <interactive>:8348:2-8: warning: [-Wtype-defaults]
--     • Defaulting the following constraint to type ‘()’
--         Show a0 arising from a use of ‘print’
--     • In a stmt of an interactive GHCi command: print it
-- *** Exception: Prelude.tail: empty list
--
