module Data.List.Zalgo
	(
		zFun,
		isInfixOf,
		indexOf,

		zFunBy,
		isInfixBy,
		indexBy,

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

zFun :: Eq a => [a] -> [Int]
zFun xs = map zLength $ zTraverse xs

isInfixOf :: Eq a => [a] -> [a] -> Bool
isInfixOf n h = isJust $ indexOf n h

indexOf :: Eq a => [a] -> [a] -> Maybe Int
indexOf n h = go $ zFun $ joinLists n h
	where
		ln = length n
		go [] = Nothing
		go (l:ls)
			| l == ln = Just (-l-l)
			| otherwise = fmap (+ 1) $ go ls

zFunBy :: (a -> a -> Bool) -> [a] -> [Int]
zFunBy eq xs = map zLength $ zTraverseBy eq xs

isInfixBy :: (a -> a -> Bool) -> [a] -> [a] -> Bool
isInfixBy eq n h = isJust $ indexBy eq n h

indexBy :: (a -> a -> Bool) -> [a] -> [a] -> Maybe Int
indexBy eq n h = go $ zFunBy (maybeEq eq) $ joinLists n h
	where
		ln = length n
		go [] = Nothing
		go (l:ls)
			| l == ln = Just (-l-l)
			| otherwise = fmap (+ 1) $ go ls
		maybeEq eq (Just x) (Just y) = x `eq` y
		maybeEq _ _ _ = False

genericZFun :: (Num i, Eq a) => [a] -> [i]
genericZFun xs = map (fromMaybe 0 . gzLength) $ gzTraverse xs

genericIndexOf :: (Eq i, Num i, Eq a) => [a] -> [a] -> Maybe i
genericIndexOf n h = go $ genericZFun $ joinLists n h
	where
		ln = genericLength n
		go [] = Nothing
		go (l:ls)
			| l == ln = Just (-l-l)
			| otherwise = fmap (+ 1) $ go ls

genericZFunBy :: Num i => (a -> a -> Bool) -> [a] -> [i]
genericZFunBy eq xs = map (fromMaybe 0 . gzLength) $ gzTraverseBy eq xs

genericIndexBy :: (Eq i, Num i) => (a -> a -> Bool) -> [a] -> [a] -> Maybe i
genericIndexBy eq n h = go $ genericZFunBy (maybeEq eq) $ joinLists n h
	where
		ln = genericLength n
		go [] = Nothing
		go (l:ls)
			| l == ln = Just (-l-l)
			| otherwise = fmap (+ 1) $ go ls
		maybeEq eq (Just x) (Just y) = x `eq` y
		maybeEq _ _ _ = False
