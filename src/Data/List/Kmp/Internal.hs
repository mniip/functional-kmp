--------------------------------------------------------------------------------
-- |
-- Module      :  Data.List.Kmp.Internal
-- Copyright   :  (C) 2015 mniip
-- License     :  BSD3
-- Maintainer  :  mniip <mniip@mniip.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Implementation of the prefix-function on cons-cells.
--
-- prefix-function has a simple implementation using arrays, the challenge,
-- however was to implement it on haskell lists (which are cons cells) without
-- losing any complexity. The following code uses a tying-the-knot data passing
-- structure, however no complicated laziness mechanisms are used: a 'KmpState'
-- only depends on previous 'KmpState's and therefore this algorithm can be
-- safely implemented in a strict language using pointers.
--------------------------------------------------------------------------------

module Data.List.Kmp.Internal
    (
        KmpState(..),
        GenericKmpState(..),
        kmpTraverse,
        kmpTraverseBy,
        gKmpTraverse,
        gKmpTraverseBy
    )
    where

import Data.Maybe

-- | A state of the prefix-function computation. 'kmpTail' points to the tail of
-- the input at the state's position, 'kmpLength' is the value of the
-- prefix-function, and 'kmpPrev' is a reference to the list of 'KmpState's
-- starting from position described by 'kmpLength'.
data KmpState a = KmpState { kmpTail :: [a], kmpLength :: !Int, kmpPrev :: [KmpState a] }

-- | /O(N)./ Compute the list of prefix-function states for a given input.
kmpTraverse :: Eq a => [a] -> [KmpState a]
kmpTraverse [] = []
kmpTraverse xw@(_:xs) = let
        pk = KmpState{kmpTail = xw, kmpLength = 0, kmpPrev = pks}
        k = KmpState{kmpTail = xs, kmpLength = 0, kmpPrev = pks}
        pks = pk:ks
        ks = k:go k xs
    in ks
    where
        go _ [] = []
        go pk (x:xs) = let k = recurse pk x xs
            in k:go k xs
        recurse KmpState{kmpLength = kl, kmpPrev = kpw@(kp@KmpState{kmpTail = kpt:_}:kps)} x xs
            | kpt == x = KmpState{kmpTail = xs, kmpLength = kl + 1, kmpPrev = kps}
            | kl == 0 = KmpState{kmpTail = xs, kmpLength = 0, kmpPrev = kpw}
            | otherwise = recurse kp x xs

-- | /O(N) and O(N) calls to the predicate./ Compute the list of prefix-function
-- states using a given equality predicate. See 'Data.List.Kmp.prefixFunBy'
-- for a detailed explanation of what predicates are allowed.
kmpTraverseBy :: (a -> a -> Bool) -> [a] -> [KmpState a]
kmpTraverseBy eq [] = []
kmpTraverseBy eq xw@(_:xs) = let
        pk = KmpState{kmpTail = xw, kmpLength = 0, kmpPrev = pks}
        k = KmpState{kmpTail = xs, kmpLength = 0, kmpPrev = pks}
        pks = pk:ks
        ks = k:go eq k xs
    in ks
    where
        go _ _ [] = []
        go eq pk (x:xs) = let k = recurse eq pk x xs
            in k:go eq k xs
        recurse eq KmpState{kmpLength = kl, kmpPrev = kpw@(kp@KmpState{kmpTail = kpt:_}:kps)} x xs
            | kpt `eq` x = KmpState{kmpTail = xs, kmpLength = kl + 1, kmpPrev = kps}
            | kl == 0 = KmpState{kmpTail = xs, kmpLength = 0, kmpPrev = kpw}
            | otherwise = recurse eq kp x xs

-- | 'KmpState' generalized over the number type. 'Nothing' represents zero and
-- is used to avoid the 'Eq' constraint
data GenericKmpState i a = GenericKmpState { gKmpTail :: [a], gKmpLength :: Maybe i, gKmpPrev :: [GenericKmpState i a] }

-- | /O(N) and O(N) plus-1's./ Compute the list of prefix-function states using
-- a given number type.
gKmpTraverse :: (Num i, Eq a) => [a] -> [GenericKmpState i a]
gKmpTraverse [] = []
gKmpTraverse xw@(_:xs) = let
        pk = GenericKmpState{gKmpTail = xw, gKmpLength = Nothing, gKmpPrev = pks}
        k = GenericKmpState{gKmpTail = xs, gKmpLength = Nothing, gKmpPrev = pks}
        pks = pk:ks
        ks = k:go k xs
    in ks
    where
        go _ [] = []
        go pk (x:xs) = let k = recurse pk x xs
            in k:go k xs
        recurse GenericKmpState{gKmpLength = kl, gKmpPrev = kpw@(kp@GenericKmpState{gKmpTail = kpt:_}:kps)} x xs
            | kpt == x = GenericKmpState{gKmpTail = xs, gKmpLength = Just $ maybe 1 (+ 1) kl, gKmpPrev = kps}
            | isNothing kl = GenericKmpState{gKmpTail = xs, gKmpLength = Just 0, gKmpPrev = kpw}
            | otherwise = recurse kp x xs

-- | /O(N), O(N) calls to the predicate, and O(N) plus-1's./ Compute the list of
-- prefix-function states using a given number type and a given equality
-- predicate.
gKmpTraverseBy :: Num i => (a -> a -> Bool) -> [a] -> [GenericKmpState i a]
gKmpTraverseBy eq [] = []
gKmpTraverseBy eq xw@(_:xs) = let
        pk = GenericKmpState{gKmpTail = xw, gKmpLength = Nothing, gKmpPrev = pks}
        k = GenericKmpState{gKmpTail = xs, gKmpLength = Nothing, gKmpPrev = pks}
        pks = pk:ks
        ks = k:go eq k xs
    in ks
    where
        go _ _ [] = []
        go eq pk (x:xs) = let k = recurse eq pk x xs
            in k:go eq k xs
        recurse eq GenericKmpState{gKmpLength = kl, gKmpPrev = kpw@(kp@GenericKmpState{gKmpTail = kpt:_}:kps)} x xs
            | kpt `eq` x = GenericKmpState{gKmpTail = xs, gKmpLength = Just $ maybe 1 (+ 1) kl, gKmpPrev = kps}
            | isNothing kl = GenericKmpState{gKmpTail = xs, gKmpLength = Just 0, gKmpPrev = kpw}
            | otherwise = recurse eq kp x xs
