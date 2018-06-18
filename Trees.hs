module Trees where

data Tree a = T a [Tree a]
    deriving Show

-- | Example
t = T 2 [ T 3 []
          , T 4 [ T 5 []
                  , T 6 []
                  ]
          , T 7 []
          ]


foldTree :: (a -> [b] -> b) -> Tree a -> b
foldTree f (T x cl) = f x (map (foldTree f) cl)

sizeTree :: Num b => Tree a -> b
sizeTree t = foldTree f t
    where
        f _ cl = 1 + (sum cl)

heightTree :: (Ord b, Num b) => Tree a -> b
heightTree t = foldTree f t 
    where
        f _ [] = 1
        f _ cl = 1 + (maximum cl)

sumTree :: Num a => Tree a -> a
sumTree t = foldTree (\x -> \cl -> x+(sum cl)) t

maxTree :: Ord a => Tree a -> a
maxTree t = foldTree f t
    where
        f x [] = x
        f x cl = maximum (x:cl)

inTree :: Eq a => a -> Tree a -> Bool
inTree x t = foldTree (\a -> \cl -> elem True ((x==a):cl)) t

nodes :: Tree a -> [a]
nodes t = foldTree (\x -> \cl -> (x:(foldl (++) [] cl))) t

countTree :: (a -> Bool) ->Tree a -> Integer
countTree f t = foldTree (\x -> \cl -> (if f x then 1 else 0 )+(sum cl)) t

leaves :: Tree a -> [a]
leaves t = foldTree (\x -> \cl -> if null cl then [x] else (foldl (++) [] cl) ) t

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f t = foldTree (\x -> \cl -> (T (f x) cl)) t

-- n=0 gives root, n=1 gives up to 1st level, and so on,
-- or else the first question on Ex 3.4 makes no sense.
trimTree :: Int -> Tree a -> Tree a
trimTree n t =
    let 
        tth d (T a ch) = 
            if d == n then
                T a []
            else
                T a (map (tth (d+1)) ch)
    in
        tth 0 t

path :: [Int] -> Tree a -> a
path l t =
    let
        pint [] (T a _) = a
        pint (n:tl) (T _ ch) = pint tl (ch!!n)
    in
        pint l t
