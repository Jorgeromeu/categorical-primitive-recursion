module InfBinTree where

import BinTree

root :: BinTree a -> a
root (Node a _ _) = a
root _ = undefined

leftChild :: BinTree a -> BinTree a
leftChild (Node _ l _) = l
leftChild _ = undefined

rightChild :: BinTree a -> BinTree a
rightChild (Node _ _ r) = r
rightChild _ = undefined

upTo :: Int -> BinTree a -> BinTree a
upTo 0 (Leaf a)       = (Leaf a)
upTo 0 (Node a _ _ )  = (Leaf a)
upTo _ (Leaf a)       = (Leaf a)
upTo n (Node a tl tr) = (Node a (upTo (n-1) tl) (upTo (n-1) tr))

tree = Node 1 (Node 2 (Node 4 (Leaf 6) (Leaf 7)) (Leaf 5)) (Leaf 3)

ana :: (x -> a) -> (x -> x) -> (x -> x) -> (x -> BinTree a)
(ana rt l r) x = Node root leftTree rightTree
    where
        root = rt x
        gen = ana rt l r
        leftTree = gen (l x)
        rightTree = gen (r x)

iter :: (a -> a) -> (a -> a) -> (a -> BinTree a)
iter fl fr = ana id fl fr

iterOr :: (a -> Either a (BinTree a)) -> (a -> Either a (BinTree a)) -> (a -> BinTree a)
iterOr sl sr = apo id sl sr

clone :: BinTree a -> BinTree a
clone = ana root leftChild rightChild 

mapTree :: (a -> b) -> BinTree a -> BinTree b
mapTree f = ana (f . root) leftChild rightChild 

apo :: (x -> a) -> (x -> Either x (BinTree a)) -> (x -> Either x (BinTree a)) -> (x -> BinTree a)
(apo rt l r) x = Node root leftTree rightTree
    where
        root = rt x
        gen = apo rt l r
        leftTree = case (l x) of
            Left x' -> gen x'
            Right t -> t
        
        rightTree = case (r x) of
            Left x' -> gen x'
            Right t -> t

depths :: BinTree Int
depths = iter (+1) (+1) 0

incs :: Int -> BinTree Int
incs x = iter (\v -> v-1) (+1) x

binstrs :: BinTree [Int]
binstrs = iter (\x -> 0:x) (\x -> 1:x) []


