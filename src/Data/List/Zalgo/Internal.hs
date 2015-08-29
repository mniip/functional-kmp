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

data ZState a = ZState { zTail :: [a], zLength :: !Int, zPrev :: [ZState a] }

zTraverse :: Eq a => [a] -> [ZState a]
zTraverse [] = []
zTraverse xw@(_:xs) = let
		pz = ZState{zTail = xw, zLength = 0, zPrev = pzs}
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

data GenericZState i a = GenericZState { gzTail :: [a], gzLength :: Maybe i, gzPrev :: [GenericZState i a] }

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
