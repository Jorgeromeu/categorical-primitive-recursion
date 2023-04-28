module Nat where

data Nat = 
      Zero 
    | Succ Nat

instance Num Nat where
        n + m = add n m
        n * m = mul n m
        fromInteger i = fromInt (fromIntegral i)

fromInt :: Int -> Nat
fromInt 0 = Zero
fromInt n = Succ (fromInt (n-1))

toInt :: Nat -> Int
toInt Zero = 0
toInt (Succ n) = 1 + (toInt n)

instance Show Nat where
    show n = show $ toInt n

add :: Nat -> Nat -> Nat
add n m = Nat.repeat (Succ) m n

mul :: Nat -> Nat -> Nat
mul n m = Nat.repeat (add n) Zero m

cata :: (x -> x) -> x -> (Nat -> x)
cata _ z Zero     = z
cata s z (Succ n) = s (cata s z n)

repeat :: (x -> x) -> x -> Nat -> x
repeat = cata

fib' :: Nat -> (Nat, Nat)
fib' Zero     = (1, 0)
fib' (Succ n) = (sub1 + sub2, sub1)
    where 
        (sub1, sub2) = fib' n

-- >>> fst $ fib' 19
-- 6765
--

-- pow2:: Nat -> Nat 
-- pow2 Zero = fromInt 1
-- pow2 n = 1 + sum [pow2 i | i <- [0..10]]