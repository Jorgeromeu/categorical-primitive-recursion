module Lists where

tails :: [a] -> [[a]]
tails []     = []
tails (x:xs) = xs:(tails xs)

tails' :: [a] -> [[a]]
tails' = para [] (\x r xs -> xs:r)

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ []     = []
dropWhile p (x:xs) = if p x then (Lists.dropWhile p xs) else x:xs

tailsLongerThan :: Int -> [a] -> [[a]]
tailsLongerThan n l = filter (\l -> length l >= n) (tails l)

tailsLongerThan' :: Int -> [a] -> [[a]]
tailsLongerThan' n = para [] (\x r xs -> if (length xs >= n) then xs:r else r)

-- >>> tailsLongerThan' 2 [1,2,3,4,5,6,7]
-- <interactive>:8503:2-35: warning: [-Wt, handoutype-defaults]
--     • Defaulting the following constraints to type ‘Integer’
--         (Show a0) arising from a use of ‘print’ at <interactive>:8503:2-35
--         (Num a0) arising from a use of ‘it’ at <interactive>:8503:2-35
--     • In a stmt of an interactive GHCi command: print it
-- [[2,3,4,5,6,7],[3,4,5,6,7],[4,5,6,7],[5,6,7],[6,7]]
--

cata :: b -> (a -> b -> b) -> [a] -> b
cata n c = foldr c n

foldWithTail :: (a -> [a] -> b -> b) -> b -> ([a] -> b)
foldWithTail op n = para n (\x r xs -> op x xs r)

-- >>> foldWithTail (\ x r xs -> xs:r) [] [1,2,3]
-- <interactive>:10191:2-43: warning: [-Wtype-defaults]
--     • Defaulting the following constraints to type ‘Integer’
--         (Show a0) arising from a use of ‘print’ at <interactive>:10191:2-43
--         (Num a0) arising from a use of ‘it’ at <interactive>:10191:2-43
--     • In a stmt of an interactive GHCi command: print it
-- <BLANKLINE>
-- <interactive>:10191:18: warning: [-Wunused-matches]
--     Defined but not used: ‘x’
-- [[2,3],[3],[]]
--

para :: b -> (a -> b -> [a] -> b) -> ([a] -> b)
(para c _)  [] = c
(para c h) (x:xs) = h x ((para c h) xs) xs

-- >>> foldWi

-- >>> [1,2,3]
-- <interactive>:5812:2-8: warning: [-Wtype-defaults]
--     • Defaulting the following constraints to type ‘Integer’
--         (Show a0) arising from a use of ‘print’ at <interactive>:5812:2-8
--         (Num a0) arising from a use of ‘it’ at <interactive>:5812:2-8
--     • In a stmt of an interactive GHCi command: print it
-- [1,2,3]
--
