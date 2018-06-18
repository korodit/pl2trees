import Trees
import Data.Char
import Test.QuickCheck

-- Checking that the new arbitrary instance works
sof = do
    x<-generate arbitrary :: IO (Tree Int)
    print $ sizeTree x

-- 
instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary =
    sized arbitrarySizedTree
    where
        arbitrarySizedTree :: Arbitrary a => Int -> Gen (Tree a)
        arbitrarySizedTree m = do
          t <- arbitrary
          newval <- choose (1,5)
          n <- choose (0, m `div` newval)
          newval <- choose (2,5)
          ts <- vectorOf n (arbitrarySizedTree (m `div` newval))
          return (T t ts)

tt = T 2 [ T 3 []
          , T 4 [ T 5 []
                  , T 6 []
                  ]
          , T 7 []
          ]

testNum = 500
testPattern x = (quickCheckWith stdArgs {maxSuccess = testNum} x)

-- Check that height if a tree has positive value which is less thant
-- the value of the size of the tree
prop_qcheight_size::Arbitrary a=>Tree a->Bool
prop_qcheight_size x = (ht>0) && (ht <= (sizeTree x))
            where
                ht = (heightTree x)

t1 = testPattern (prop_qcheight_size::Tree Integer->Bool)
-----------------------
-- Check that the tree's max element belongs to Tree
prop_max_intree::(Arbitrary a,Ord a)=>Tree a->Bool
prop_max_intree x = inTree (maxTree x) x

t2 = testPattern (prop_max_intree::Tree Integer->Bool)
-----------------------
-- Check that every node belongs to the tree
prop_allnodes_intree::(Arbitrary a,Eq a)=>Tree a->Bool
prop_allnodes_intree x = and (map (\e->inTree e x) (nodes x))

t3 = testPattern (prop_max_intree::Tree Integer->Bool)
-----------------------
-- Check that the number of nodes that satisfy
-- a predicate is smaller than the size of the tree
prop_pred_lim::Arbitrary a=>(a->Bool)->Tree a->Bool
prop_pred_lim pred x = (countTree pred x) <= (sizeTree x)

-- Testing with predicate odd
t4 = testPattern (prop_pred_lim (odd::Integer->Bool))
-----------------------
-- Check that the number of nodes of a tree equals its size,
-- and that the number of the tree's leaves is smaller than
-- the number of its nodes, except if the number of nodes
-- is one, in which case they are equal in number
prop_len_size_leafs::Arbitrary a=>Tree a->Bool
prop_len_size_leafs x = (ln == st) &&
                            (
                            ((ln == 1) && (ll == 1))
                            || 
                            (ln > ll)
                            )
                            where
                              st = sizeTree x
                              ll = length (leaves x)
                              ln = length (nodes x)

t5 = testPattern (prop_len_size_leafs::Tree Integer->Bool)
-----------------------
-- Check that mapTree function preserves the size and
-- the height of the tree
prop_map_size::(a->b)->Tree a->Bool
prop_map_size mapfun x = ((heightTree mt) == (heightTree x)) && ((sizeTree mt) == (sizeTree x))
                          where
                            mt = mapTree mapfun x

-- Testing with function to map (*2)
t6 = testPattern (prop_map_size ((*2)::Integer->Integer))
-----------------------
-- Check that if element n belongs to tree t, then the element
-- f n belongs to tree mapTree f t
prop_map_intree::(Eq a,Eq b)=>(a->b)->Tree a->Bool
prop_map_intree f x = and (map (\e->inTree e mt) (map f (nodes x)))
                        where
                          mt = (mapTree f x)

-- Testing with function to map (*2)
t7 = testPattern (prop_map_intree ((*2)::Integer->Integer))
-----------------------
-- Check that, for any function g belonging to set {nodes,leaves}
-- it is true that map f.g == g . mapTree f
prop_eq_nl:: Eq a=>(Tree a->[a])->(a->a)->Tree a->Bool
prop_eq_nl tf f x = (map f (tf x)) == (tf (mapTree f x))

-- Testing for each of the functions nodes, leaves,
-- with map function (*2)
t8 = testPattern (prop_eq_nl nodes ((* 2)::Integer->Integer))
t9 = testPattern (prop_eq_nl leaves ((* 2)::Integer->Integer))

-- Run all the tests
run_all_trees = do
  t1
  t2
  t3
  t4
  t5
  t6
  t7
  t8
  t9