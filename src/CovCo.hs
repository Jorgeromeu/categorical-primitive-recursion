module CovCo where

import Stream

data Lst c a = Singl c | Cons a (Lst c a)
    deriving (Show)

fromList :: [a] -> c -> Lst c a
fromList [] c     = Singl c
fromList (a:as) c = Cons a (fromList as c)

toList :: Lst c a -> ([a], c)
toList (Singl c)  = ([], c)
toList (Cons a l) = undefined
    where (lstTl, _) = toList l

-- >>> fromList [1,2,3] "a"
-- <interactive>:1985:2-21: warning: [-Wtype-defaults]
--     • Defaulting the following constraints to type ‘Integer’
--         (Show a0) arising from a use of ‘print’ at <interactive>:1985:2-21
--         (Num a0) arising from a use of ‘it’ at <interactive>:1985:2-21
--     • In a stmt of an interactive GHCi command: print it
-- Cons 1 (Cons 2 (Cons 3 (Singl "a")))
--
