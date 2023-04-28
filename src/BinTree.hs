module BinTree where

data BinTree a = 
      Leaf a 
    | Node a (BinTree a) (BinTree a) deriving (Show, Eq)

line :: Int -> String
line n = concat $ replicate n "--"

ident :: Int -> Int -> String
ident n m = concat $ replicate n (replicate m ' ')

showTree :: Show a => Int -> Int -> BinTree a -> String
showTree n m (Leaf x)     = ident n m ++ show x
showTree n m (Node x l r) = ident n m ++ show x ++ showChild l ++ showChild r
    where 
        showChild c = "\n" ++ ident n m ++ showTree (n+1) m c

cata :: (a -> b) -> (a -> b -> b -> b) -> (BinTree a -> b)
cata onLeaf _ (Leaf a)     = onLeaf a
cata onLeaf onNode (Node a tl tr) = onNode a (cata onLeaf onNode tl) (cata onLeaf onNode tr)

para :: (a -> b) -> (a -> b -> BinTree a -> b -> BinTree a -> b) -> (BinTree a -> b)
para onLeaf _      (Leaf a)       = onLeaf a
para onLeaf onNode (Node a tl tr) = onNode a (f tl) tl (f tr) tr
    where f = para onLeaf onNode

-- functions defined as para/catamorphisims

contains :: (Eq a) => a -> BinTree a -> Bool
contains t (Leaf a)     = a == t
contains t (Node a l r) = a == t || contains t l || contains t r

contains' :: (Eq a) => a -> BinTree a -> Bool
contains' t = cata onLeaf onNode 
    where
        onLeaf = \x -> x == t
        onNode = \x inL inR -> x == t || inL || inR

flatten :: BinTree a -> [a]
flatten (Leaf a)       = [a]
flatten (Node a tl tr) = [a] ++ flatten tl ++ flatten tr

subTrees :: BinTree a -> [BinTree a]
subTrees (Leaf a) = []
subTrees (Node a tl tr) = [tl, tr] ++ subTrees tl ++ subTrees tr

subTrees' :: BinTree a -> [BinTree a]
subTrees' = para onLeaf onNode
    where
        onLeaf = (\_ -> []) 
        onNode = (\_ subTreesL tL subTreesR tR -> [tL, tR] ++ subTreesL ++ subTreesR)
