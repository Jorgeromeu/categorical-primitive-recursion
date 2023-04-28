module CoNat where

data CoNat = 
      Zero 
    | Infty
    | Succ CoNat 

fromInt :: Int -> CoNat
fromInt 0 = Zero
fromInt n = Succ (fromInt (n-1))

toInt :: CoNat -> Maybe Int
toInt Zero = Just 0
toInt Infty = Nothing
toInt (Succ n) = case toInt n of
    Just n -> Just (n+1)
    Nothing -> Nothing

instance Show CoNat where
    show n = case toInt n of
        Just x -> show x
        Nothing -> "infty"

pred :: CoNat -> Maybe CoNat
pred Zero = Nothing
pred (Succ n) = Just n
pred Infty = Just Infty

-- ana :: (x -> Maybe x) -> (x -> CoNat)
-- ana p x = case (p x) of
--     Just x' -> () x'

