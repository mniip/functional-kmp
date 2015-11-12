--------------------------------------------------------------------------------
-- |
-- Module      :  Data.List.Kmp
-- Copyright   :  (C) 2015 mniip
-- License     :  BSD3
-- Maintainer  :  mniip <mniip@mniip.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- A few efficient list-processing functions using the prefix-function, which is
-- defined as:
--
-- > (prefixFun xs) !! i
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

module Data.List.Kmp
    (
        prefixFun,
        isInfixOf,
        indexOf,

        -- * Custom predicates
        -- $predicates
        prefixFunBy,
        isInfixBy,
        indexBy,

        -- * Generic functions
        -- $generic
        genericPrefixFun,
        genericIndexOf,

        genericPrefixFunBy,
        genericIndexBy
    )
    where

import Data.List hiding (isInfixOf)
import Data.Maybe
import Data.List.Kmp.Internal

joinLists :: [a] -> [a] -> [Maybe a]
joinLists n h = map Just n ++ Nothing:map Just h

maybeEq :: (a -> a -> Bool) -> Maybe a -> Maybe a -> Bool
maybeEq eq Nothing Nothing = True
maybeEq eq (Just a) (Just b) = a `eq` b
maybeEq eq _ _ = False

-- | /O(N)./ Compute the prefix-function for a list.
prefixFun :: Eq a => [a] -> [Int]
prefixFun xs = map kmpLength $ kmpTraverse xs

-- | /O(N+H)./ @isInfixOf needle haystack@ tests whether needle is fully
-- contained somewhere in haystack.
isInfixOf :: Eq a => [a] -> [a] -> Bool
isInfixOf n h = any (== ln) $ prefixFun $ joinLists n h
    where
        ln = length n

-- | /O(N+H)./ @indexOf needle haystack@ returns the index at which needle
-- is found in haystack, or Nothing if it's not.
indexOf :: Eq a => [a] -> [a] -> Maybe Int
indexOf n h = fmap (subtract ln . subtract ln) $ elemIndex ln $ prefixFun $ joinLists n h
    where
        ln = length n

-- $predicates
--
-- The @...By@ set of functions takes a custom equality predicate, and due to
-- the optimized nature of the algorithm, the passed predicate must conform to
-- some laws:
-- 
-- > Commutativity: a == b  =>  b == a
-- > Inverse commutativity: a /= b  =>  b /= a
-- > Transitivity: a == b and b == c  =>  a == c
-- > Inverse transitivity: a == b and b /= c  =>  a /= c
--
-- If these laws do not hold, the behavior is undefined.

-- | /O(N) and O(N) calls to the predicate./ Compute the prefix-function using a
-- custom equality predicate.
prefixFunBy :: (a -> a -> Bool) -> [a] -> [Int]
prefixFunBy eq xs = map kmpLength $ kmpTraverseBy eq xs

-- | /O(N+H) and O(N+H) calls to the predicate./ Compute 'isInfixOf' using a
-- custom equality predicate.
isInfixBy :: (a -> a -> Bool) -> [a] -> [a] -> Bool
isInfixBy eq n h = any (== ln) $ prefixFunBy (maybeEq eq) $ joinLists n h
    where
        ln = length n

-- | /O(N+H) and O(N+H) calls to the predicate./ Compute 'indexOf' using a
-- custom equality predicate.
indexBy :: (a -> a -> Bool) -> [a] -> [a] -> Maybe Int
indexBy eq n h = fmap (subtract ln . subtract ln) $ elemIndex ln $ prefixFunBy (maybeEq eq) $ joinLists n h
    where
        ln = length n

-- $generic
--
-- Some of the functions are generalized over the type of the numbers they
-- return, but keep in mind that the amount of arithmetic operations is linear.

genericPrefixFun :: (Num i, Eq a) => [a] -> [i]
genericPrefixFun xs = map (fromMaybe 0 . gKmpLength) $ gKmpTraverse xs

genericIndexOf :: (Eq i, Num i, Eq a) => [a] -> [a] -> Maybe i
genericIndexOf n h = go 0 $ genericPrefixFun $ joinLists n h
    where
        ln = genericLength n
        go n [] = Nothing
        go n (l:ls)
            | l == ln = Just (n - ln - ln)
            | otherwise = go (n + 1) ls

genericPrefixFunBy :: Num i => (a -> a -> Bool) -> [a] -> [i]
genericPrefixFunBy eq xs = map (fromMaybe 0 . gKmpLength) $ gKmpTraverseBy eq xs

genericIndexBy :: (Eq i, Num i) => (a -> a -> Bool) -> [a] -> [a] -> Maybe i
genericIndexBy eq n h = go 0 $ genericPrefixFunBy (maybeEq eq) $ joinLists n h
    where
        ln = genericLength n
        go n [] = Nothing
        go n (l:ls)
            | l == ln = Just (n - ln - ln)
            | otherwise = go (n + 1) ls
