module Colists where

unfold :: (x -> Maybe (a, x)) -> (x -> [a])
(unfold f) x = case f x of
    Just (hd, tl) -> hd : unfold f tl
    Nothing -> []
