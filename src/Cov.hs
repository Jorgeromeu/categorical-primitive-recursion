module Cov where

import Nat

cur :: [a] -> a
cur [] = undefined
cur [x] = x
cur (x:_) = x

prev :: [a] -> Maybe [a]
prev (_:xs) = Just xs
prev _ = Nothing

ana :: (x -> a) -> (x -> Maybe x) -> (x -> [a])
ana c p x = case (p x) of
    Just x' -> (c x):(ana c p x')
    Nothing -> [c x]

pred :: Nat -> Maybe Nat
pred Zero = Nothing
pred (Succ n) = Just n

fib :: Nat -> Nat
fib (Zero) = 1
fib (Succ Zero) = 1
fib (Succ x) = (fib x) + (cur $ ana fib Cov.pred x)



-- >>> ana (+2) Cov.pred 10
-- [12,11,10,9,8,7,6,5,4,3,2]
--
