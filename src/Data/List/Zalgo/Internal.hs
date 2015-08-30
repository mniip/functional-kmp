--------------------------------------------------------------------------------
-- |
-- Module      :  Data.List.Zalgo.Internal
-- Copyright   :  (C) 2015 mniip
-- License     :  BSD3
-- Maintainer  :  mniip <mniip@mniip.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Implementation of the Z-function on cons-cells.
--
-- Z-function has a simple implementation using arrays, the challenge, however
-- was to implement it on haskell lists (which are cons cells) without losing
-- any complexity. The following code uses a tying-the-knot data passing
-- structure, however no complicated laziness mechanisms are used: a 'ZState'
-- only depends on previous 'ZState's and therefore this algorithm can be safely
-- implemented in a strict language with pointers.
--------------------------------------------------------------------------------

module Data.List.Zalgo.Internal
    (
        ZState(..),
        GenericZState(..),
        zTraverse,
        zTraverseBy,
        gzTraverse,
        gzTraverseBy
    )
    where

import Data.Maybe

-- | A state of the Z-function computation. 'zTail' points to the tail of the
-- input at the state's position, 'zLength' is the value of the Z-function, and
-- 'zPrev' is a reference to the list of 'ZState's starting from position
-- described by 'zLength'.
data ZState a = ZState { zTail :: [a], zLength :: Int, zPrev :: [ZState a] }

-- | /O(N)./ Compute the list of Z-function states for a given input.
zTraverse :: Eq a => [a] -> [ZState a]
zTraverse [] = []
zTraverse xw@(_:xs) = let
        pz = ZState{zTail = xw, zLength = error "nil", zPrev = pzs}
        z = ZState{zTail = xs, zLength = 0, zPrev = pzs}
        pzs = pz:zs
        zs = z:go z xs
    in zs
    where
        go _ [] = []
        go pz (x:xs) = let z = recurse pz x xs
            in z:go z xs
        recurse ZState{zLength = zl, zPrev = zpw@(zp@ZState{zTail = zpt:_}:zps)} x xs
            | zpt == x = ZState{zTail = xs, zLength = zl + 1, zPrev = zps}
            | zl == 0 = ZState{zTail = xs, zLength = 0, zPrev = zpw}
            | otherwise = recurse zp x xs

-- | /O(N) and O(N) calls to the predicate./ Compute the list of Z-function
-- states using a given equality predicate. See "Data.List.Zalgo.zFunBy" for a
-- detailed explanation of what predicates are allowed.
zTraverseBy :: (a -> a -> Bool) -> [a] -> [ZState a]
zTraverseBy eq [] = []
zTraverseBy eq xw@(_:xs) = let
        pz = ZState{zTail = xw, zLength = 0, zPrev = pzs}
        z = ZState{zTail = xs, zLength = 0, zPrev = pzs}
        pzs = pz:zs
        zs = z:go eq z xs
    in zs
    where
        go _ _ [] = []
        go eq pz (x:xs) = let z = recurse eq pz x xs
            in z:go eq z xs
        recurse eq ZState{zLength = zl, zPrev = zpw@(zp@ZState{zTail = zpt:_}:zps)} x xs
            | zpt `eq` x = ZState{zTail = xs, zLength = zl + 1, zPrev = zps}
            | zl == 0 = ZState{zTail = xs, zLength = 0, zPrev = zpw}
            | otherwise = recurse eq zp x xs

-- | 'ZState' generalized of the number type. 'Nothing' represents zero and is
-- used to avoid the 'Eq' constraint
data GenericZState i a = GenericZState { gzTail :: [a], gzLength :: Maybe i, gzPrev :: [GenericZState i a] }

-- | /O(N) and O(N) plus-1's./ Compute the list of Z-function states using a
-- given number type.
gzTraverse :: (Num i, Eq a) => [a] -> [GenericZState i a]
gzTraverse [] = []
gzTraverse xw@(_:xs) = let
        pz = GenericZState{gzTail = xw, gzLength = Nothing, gzPrev = pzs}
        z = GenericZState{gzTail = xs, gzLength = Nothing, gzPrev = pzs}
        pzs = pz:zs
        zs = z:go z xs
    in zs
    where
        go _ [] = []
        go pz (x:xs) = let z = recurse pz x xs
            in z:go z xs
        recurse GenericZState{gzLength = zl, gzPrev = zpw@(zp@GenericZState{gzTail = zpt:_}:zps)} x xs
            | zpt == x = GenericZState{gzTail = xs, gzLength = Just $ maybe 1 (+ 1) zl, gzPrev = zps}
            | isNothing zl = GenericZState{gzTail = xs, gzLength = Nothing, gzPrev = zpw}
            | otherwise = recurse zp x xs

-- | /O(N), O(N) calls to the predicate, and O(N) plus-1's./ Compute the list of
-- Z-function states using a given number type and a given equality predicate.
gzTraverseBy :: Num i => (a -> a -> Bool) -> [a] -> [GenericZState i a]
gzTraverseBy eq [] = []
gzTraverseBy eq xw@(_:xs) = let
        pz = GenericZState{gzTail = xw, gzLength = Nothing, gzPrev = pzs}
        z = GenericZState{gzTail = xs, gzLength = Nothing, gzPrev = pzs}
        pzs = pz:zs
        zs = z:go eq z xs
    in zs
    where
        go _ _ [] = []
        go eq pz (x:xs) = let z = recurse eq pz x xs
            in z:go eq z xs
        recurse eq GenericZState{gzLength = zl, gzPrev = zpw@(zp@GenericZState{gzTail = zpt:_}:zps)} x xs
            | zpt `eq` x = GenericZState{gzTail = xs, gzLength = Just $ maybe 1 (+ 1) zl, gzPrev = zps}
            | isNothing zl = GenericZState{gzTail = xs, gzLength = Nothing, gzPrev = zpw}
            | otherwise = recurse eq zp x xs
