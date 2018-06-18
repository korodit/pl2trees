import Trees
import Data.Ratio
import Test.QuickCheck

testNum = 500
testPattern x = (quickCheckWith stdArgs {maxSuccess = testNum} x)

-- Bird tree declared in accordance to the paper given
bird:: Tree (Ratio Integer)
bird = T (1%1) [(mapTree (\x->(1/x)) (mapTree (+ 1) bird)), (mapTree (+1) (mapTree (\x->(1/x)) bird))]

-- Check that a path of length n is the same in bird and trimTree n bird
prop_pathsame::[Int]->Bool
prop_pathsame p = (path p bird) == (path p (trimTree (length p) bird))

t1 = testPattern (forAll (listOf (choose (0,1))) prop_pathsame)
------------------
-- Check that creating a path as asked yields all the positive natural numbers
prop_natural::(Positive Int)->Bool
prop_natural (Positive n) = (path (take (n-1) (cycle [1,0])) bird) == (toInteger n)%1

t2 = testPattern prop_natural
------------------
-- Chech that following the leftmost path of the tree
-- we go through the numbers of the fibonacci sequence
fibs = 0 : 1 : sumlists fibs (tail fibs)
    where
        sumlists (x:xs) (y:ys) = (x + y) : sumlists xs ys
fibnums = tail (tail fibs)

prop_fib::(Positive Int)->Bool
prop_fib (Positive n) = (denominator (path (take (n-1) (cycle [0])) bird)) == (fibnums!!(n-1))

t3 = testPattern prop_fib
------------------
-- Check that every rational number exists in the
-- tree - by finding a path towards it in the bird tree

-- Using Positive Integers instead of a ratio directly,
-- for practical, terminating Quickchecks
exists::Positive Integer -> Positive Integer -> Bool
exists (Positive a) (Positive b) = (length (find_path (a%b))) > 0

-- For the strong at heart and computational power - and mostly, patient
exists_extreme::Ratio Integer -> Bool
exists_extreme rat = (length (find_path rat)) > 0

find_path::Ratio Integer->[Ratio Integer]
find_path rat = 
    let
        crawl level p acc =
            let
                curr = path p bird
            in
                if (curr < rat) then
                    crawl (level+1) (p ++ [1 - (level `mod` 2)]) (curr:acc)
                else if (curr > rat) then 
                    crawl (level+1) (p ++ [level `mod` 2]) (curr:acc)
                else  (curr:acc)
    in
        reverse (crawl 0 [] [])

t4 = testPattern exists
t5 = testPattern exists_extreme
------------------
-- Run all the tests
run_all_bird = do
  t1
  t2
  t3
  t4
  -- t5