--------------------------------------------------------------------------------
-- |
-- Module      :  Data.List.Zalgo
-- Copyright   :  (C) 2015 mniip
-- License     :  BSD3
-- Maintainer  :  mniip <mniip@mniip.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- A few efficient list-processing functions using the Z-function, which is
-- defined as:
--
-- > (z xs) !! i
--
-- is the length of the largest proper substring of @xs@ ending at position @i@,
-- such that it equals the beginning of @xs@.
--
-- For example:
--
-- > .-----.             .-----.
-- > a b a c a b a a a b a b a c d
-- > 0 0 1 0 1 2 3 1 1 2 3 2 3 4 0
-- >                           ^
--
-- The marked substrings are equal, hence the value at the marked location is
-- their length, 4.
--------------------------------------------------------------------------------

module Data.List.Zalgo
    (
        zFun,
        isInfixOf,
        indexOf,

        -- * Custom predicates
        -- $predicates
        zFunBy,
        isInfixBy,
        indexBy,

        -- * Generic functions
        -- $generic
        genericZFun,
        genericIndexOf,

        genericZFunBy,
        genericIndexBy
    )
    where

import Data.List hiding (isInfixOf)
import Data.Maybe
import Data.List.Zalgo.Internal

joinLists :: [a] -> [a] -> [Maybe a]
joinLists n h = map Just n ++ Nothing:map Just h

-- | /O(N)./ Compute the Z-function for a list.
zFun :: Eq a => [a] -> [Int]
zFun xs = map zLength $ zTraverse xs

-- | /O(N+H)./ @isInfixOf needle haystack@ tests whether needle is fully
-- contained somewhere in haystack.
isInfixOf :: Eq a => [a] -> [a] -> Bool
isInfixOf n h = isJust $ indexOf n h

-- | /O(N+H)./ @indexOf needle haystack@ returns the index at which needle
-- is found in haystack, or Nothing if it's not.
indexOf :: Eq a => [a] -> [a] -> Maybe Int
indexOf n h = go 0 $ zFun $ joinLists n h
    where
        ln = length n
        go n [] = Nothing
        go n (l:ls)
            | l == ln = Just (n - ln - ln)
            | otherwise = n `seq` go (n + 1) ls

-- $predicates
--
-- The @...By@ set of functions takes a custom equality predicate, and due to
-- the optimized nature of the algorithm, passed predicate must conform to
-- some laws:
-- 
-- > Commutativity: a == b  =>  b == a
-- > Inverse commutativity: a /= b  =>  b /= a
-- > Transitivity: a == b and b == c  =>  a == c
-- > Inverse transitivity: a == b and b /= c  =>  a /= c
--
-- If these laws do not hold, the behavior is undefined.

-- | /O(N) and O(N) calls to the predicate./ Compute the Z-function using a
-- custom equality predicate.
zFunBy :: (a -> a -> Bool) -> [a] -> [Int]
zFunBy eq xs = map zLength $ zTraverseBy eq xs

-- | /O(N+H) and O(N+H) calls to the predicate./ Compute 'isInfixOf' using a
-- custom equality predicate.
isInfixBy :: (a -> a -> Bool) -> [a] -> [a] -> Bool
isInfixBy eq n h = isJust $ indexBy eq n h

-- | /O(N+H) and O(N+H) calls to the predicate./ Compute 'indexOf' using a
-- cusom equality predicate.
indexBy :: (a -> a -> Bool) -> [a] -> [a] -> Maybe Int
indexBy eq n h = go 0 $ zFunBy (maybeEq eq) $ joinLists n h
    where
        ln = length n
        go n [] = Nothing
        go n (l:ls)
            | l == ln = Just (n - ln - ln)
            | otherwise = n `seq` go (n + 1) ls
        maybeEq eq (Just x) (Just y) = x `eq` y
        maybeEq _ _ _ = False

-- $generic
--
-- Some of the functions are generalized over the type of the numbers they
-- return, but keep in mind that the amount of arithmetic operations is linear.

genericZFun :: (Num i, Eq a) => [a] -> [i]
genericZFun xs = map (fromMaybe 0 . gzLength) $ gzTraverse xs

genericIndexOf :: (Eq i, Num i, Eq a) => [a] -> [a] -> Maybe i
genericIndexOf n h = go 0 $ genericZFun $ joinLists n h
    where
        ln = genericLength n
        go n [] = Nothing
        go n (l:ls)
            | l == ln = Just (n - ln - ln)
            | otherwise = n `seq` go (n + 1) ls

genericZFunBy :: Num i => (a -> a -> Bool) -> [a] -> [i]
genericZFunBy eq xs = map (fromMaybe 0 . gzLength) $ gzTraverseBy eq xs

genericIndexBy :: (Eq i, Num i) => (a -> a -> Bool) -> [a] -> [a] -> Maybe i
genericIndexBy eq n h = go 0 $ genericZFunBy (maybeEq eq) $ joinLists n h
    where
        ln = genericLength n
        go n [] = Nothing
        go n (l:ls)
            | l == ln = Just (n - ln - ln)
            | otherwise = n `seq` go (n + 1) ls
        maybeEq eq (Just x) (Just y) = x `eq` y
        maybeEq _ _ _ = False
