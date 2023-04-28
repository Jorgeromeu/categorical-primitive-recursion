module Stream where

ana :: (x -> a) -> (x -> x) -> (x -> [a])
(ana h t) x = hd : tl
    where 
        hd = (h x)
        tl = (ana h t) (t x)

-- more intuitive version of ana 
iterate :: (a -> a) -> (a -> [a])
iterate f = ana id f

-- >>> take 10 $ Prelude.iterate (+1) 0
-- <interactive>:1551:2-33: warning: [-Wtype-defaults]
--     • Defaulting the following constraints to type ‘Integer’
--         (Show a0) arising from a use of ‘print’ at <interactive>:1551:2-33
--         (Num a0) arising from a use of ‘it’ at <interactive>:1551:2-33
--     • In a stmt of an interactive GHCi command: print it
-- [0,1,2,3,4,5,6,7,8,9]
--

apo :: (x -> a) -> (x -> Either x [a]) -> (x -> [a])
(apo h t) x = (h x) : tl
    where 
        tl = case t x of
            -- x' is next seed
            Left x'   -> (apo h t) x'
            -- remainder
            Right as  -> as

apo' :: (x -> Bool) -> (x -> a) -> (x -> x) -> (x -> [a]) -> (x -> [a])
(apo' p h t r) x = (h x) : if (p x) then (apo' p h t r) (t x) else (r x)

-- analogous to iter, but with primitive corecursion
iterateOr :: (a -> Either a [a]) -> (a -> [a])
iterateOr f = apo id f

-- Example: iterateOr
-- ==================
-- >>> take 10 $ Prelude.iterate (+2) 0 
-- <interactive>:423:2-33: warning: [-Wtype-defaults]
--     • Defaulting the following constraints to type ‘Integer’
--         (Show a0) arising from a use of ‘print’ at <interactive>:423:2-33
--         (Num a0) arising from a use of ‘it’ at <interactive>:423:2-33
--     • In a stmt of an interactive GHCi command: print it
-- [0,2,4,6,8,10,12,14,16,18]
--

-- >>> take 30 $ iterateOr (\x -> if x < 3 then Left $ x+1 else Right zeros) 0
-- [0,1,2,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
--

iterateOr' :: (a -> Either a [a]) -> (a -> [a])
iterateOr' f s = s:tl
    where
        tl = case (f s) of
            Left s' -> iterateOr' f s'
            Right rest -> rest

iterWhileThen :: (a -> Bool) -> (a -> a) -> [a] -> (a -> [a])
iterWhileThen p f r = apo' p id f (\_ -> r)

iterWhileThen' :: (a -> Bool) -> (a -> a) -> [a] -> (a -> [a])
iterWhileThen' p f rest s = if (p s) then s:(iterWhileThen' p f rest (f s)) else s:rest

-- EXAMPLE
-- >>> take 10 $ iterWhileThen (<5) (+1) (zeros) (02)


mapInf :: (a -> b) -> [a] -> [b]
mapInf f = ana (\xs -> f (head xs)) tail

insert :: (Ord a) => a -> [a] -> [a]
insert a = apo h t
    where 
        h xs = min (head xs) a
        t xs = if (head xs < a) then Left (tail xs) else Right (xs)

-- EXAMPLE:
-- >>> take 10 $ insert 5 $ insert 7 $ nats 0
-- [0,1,2,3,4,5,5,6,7,7]
--

removeFst :: (Eq a) => a -> [a] -> [a]
removeFst a = apo h t
    where 
        h xs = if (a == (head xs)) then (head (tail xs)) else (head xs)
        t xs = if (a == (head xs)) then Right (tail (tail xs)) else Left (tail xs)

-- >>> take 10 $ removeFst 7 $ nats 0
-- [0,1,2,3,4,5,6,8,9,10]
--

-- EXAMPLE:
-- >>> take 10 $ replace 5 42 $ nats 0 
-- [0,1,2,3,4,42,6,7,8,9]
--

replace :: (Eq a) => a -> a -> [a] -> [a]
replace old new = apo h t
    where 
        h xs = if (old == (head xs)) then new else (head xs)
        t xs = if (old == (head xs)) then Right (tail xs) else Left (tail xs)

mapHd :: (a -> a) -> [a] -> [a]
mapHd f = apo (f . head) (\xs -> Right $ tail xs)

mapWhileOr :: (a -> Bool) -> (a -> a) -> ([a] -> [a]) -> [a] -> [a]
mapWhileOr p f r = apo hd tl
    where
        hd xs = if (p x) then mappedHead else remainderHead 
            where 
                x = head xs
                mappedHead = f x
                remainderHead = head (r xs)

        tl xs = if (p x) then Left mappedTail else Right remainderTail
            where
                x = head xs
                mappedTail = tail xs
                remainderTail = tail (r xs)

mapWhile :: (a -> Bool) -> (a -> a) -> ([a] -> [a])
mapWhile p f = mapWhileOr p f id

-- Example: mapWhile
-- >>> take 100 $ mapWhile (<5) (*100) $ nats 0
-- [0,100,200,300,400,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99]
--

mapWhileThen :: (a -> Bool) -> (a -> a) -> (a -> a) -> ([a] -> [a])
mapWhileThen p f g = mapWhileOr p f (mapInf g)

replaceAfter :: (a -> Bool) -> [a] -> [a] -> [a]
replaceAfter p r = mapWhileOr (not . p) id (\_ -> r)

cloneApo :: [Int] -> [Int]
cloneApo = apo head (\xs -> Left (tail xs))

-- >>> take 10 $ cloneApo (nats 0)
-- [0,1,2,3,4,5,6,7,8,9]
--

nats :: Int -> [Int]
nats = ana id (+1)

repeat :: a -> [a]
repeat = ana id id

zeros :: [Int]
zeros = Stream.repeat 0

zipInf :: [a] -> [b] -> [(a, b)]
zipInf as bs = ana h t (as, bs)
    where 
        h = (\(xs, ys) -> (head xs, head ys))
        t = (\(xs, ys) -> (tail xs, tail ys))
