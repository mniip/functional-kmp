{-# LANGUAGE RankNTypes #-}
module Test where

import Test.Hspec
import Test.QuickCheck

import Data.Function
import qualified Data.List

import Data.List.Kmp

prefixFunSpec :: (forall a. Eq a => [a] -> [Int]) -> SpecWith ()
prefixFunSpec f = do
    it "computes the prefix-function" $ do
        f "abacaba" `shouldBe` [0, 0, 1, 0, 1, 2, 3]

    it "works on empty lists" $ do
        f ([] :: [()]) `shouldBe` []

    it "returns lists of appropriate length" $
        property $ \as -> length (f (as :: [Int])) == length as

    context "works on short lists of ints" $ do
        it "works on [A]" $ property $ \a -> f [a :: Int] == [0]
        it "works on [A, A]" $ property $ \a -> f [a :: Int, a] == [0, 1]
        it "works on [A, B]" $ property $ \a b -> (a /= b) ==> f [a :: Int, b :: Int] == [0, 0]
        it "works on [A, A, A]" $ property $ \a -> f [a :: Int, a, a] == [0, 1, 2]
        it "works on [A, B, B]" $ property $ \a b -> (a /= b) ==> f [a :: Int, b :: Int, b] == [0, 0, 0]
        it "works on [A, A, B]" $ property $ \a b -> (a /= b) ==> f [a :: Int, a, b :: Int] == [0, 1, 0]
        it "works on [A, B, A]" $ property $ \a b -> (a /= b) ==> f [a :: Int, b :: Int, a] == [0, 0, 1]
        it "works on [A, B, C]" $ property $ \a b c -> (a /= b && b /= c && a /= c) ==> f [a :: Int, b :: Int, c :: Int] == [0, 0, 0]

    it "handles series" $ do
        f "aaaabaaaaaaaaaa" `shouldBe` [0, 1, 2, 3, 0, 1, 2, 3, 4, 4, 4, 4, 4, 4, 4]
        f "baaaa" `shouldBe` [0, 0, 0, 0, 0]

    it "handles partial backtracks" $ do
        f "abac|ababab" `shouldBe` [0, 0, 1, 0, 0, 1, 2, 3, 2, 3, 2]
        f "abacabad|abacabad" `shouldBe` [0, 0, 1, 0, 1, 2, 3, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8]
        f "abacabad|abacabac" `shouldBe` [0, 0, 1, 0, 1, 2, 3, 0, 0, 1, 2, 3, 4, 5, 6, 7, 4]
        f "abacabad|abacabab" `shouldBe` [0, 0, 1, 0, 1, 2, 3, 0, 0, 1, 2, 3, 4, 5, 6, 7, 2]

prefixFunBySpec :: (forall a. (a -> a -> Bool) -> [a] -> [Int]) -> SpecWith ()
prefixFunBySpec f = do
    context "mimicks prefixFun when applied to (==)" $ do
        prefixFunSpec $ f (==)

    it "mimicks prefixFun with a forgetful predicate" $
        property $ \as -> f ((==) `on` fst) (zip as [1..]) == prefixFun (as :: [Bool])

    it "returns increasing numbers with a constantly True predicate" $
        f (const $ const True) "asdfasd" `shouldBe` [0, 1, 2, 3, 4, 5, 6]

isInfixOfSpec :: (forall a. Eq a => [a] -> [a] -> Bool) -> SpecWith ()
isInfixOfSpec f = do
    it "tests whether needle is in the haystack" $ do
        f "[needle]" "hello[needle]world" `shouldBe` True
        f "[needle]" "some stuff" `shouldBe` False

    it "returns True for empty needles" $
        property $ \as -> f [] (as :: [Bool])

    it "returns False for empty haystacks" $
        property $ \as -> not (null as) ==> not (f (as :: [Bool]) [])

    context "works on lists of Bool" $ do
        it "returns True when needle is in the haystack" $
            property $ \as bs cs -> f (as :: [Bool]) (bs ++ as ++ cs)

        it "is identical to Data.List.isInfixOf" $
            property $ \as bs -> f (as :: [Bool]) bs == Data.List.isInfixOf as bs

isInfixBySpec :: (forall a. (a -> a -> Bool) -> [a] -> [a] -> Bool) -> SpecWith ()
isInfixBySpec f = do
    context "mimicks isInfixOf when applied to (==)" $ do
        isInfixOfSpec $ f (==)

    it "mimicks isInfixOf with a forgetful predicate" $
        property $ \as bs -> f ((==) `on` fst) (zip (as :: [Bool]) [1..]) (zip bs [1..]) == isInfixOf as bs

    it "only compares length if predicate is constantly true" $
        property $ \as bs -> f (const $ const True) (as :: [Bool]) bs == (length as <= length bs)

indexOfSpec :: (forall a. Eq a => [a] -> [a] -> Maybe Int) -> SpecWith ()
indexOfSpec f = do
    it "finds a needle in the haystack" $ do
        f "[needle]" "hello[needle]world" `shouldBe` Just 5
        f "[needle]" "some stuff" `shouldBe` Nothing

    it "finds a needle" $
        property $ \as bs -> case f (as :: [Bool]) bs of
            Just i -> Data.List.isPrefixOf as $ drop i bs 
            Nothing -> True

    it "finds the first needle in the haystack" $ do
        f "[needle]" "h[needle]el[needle]o[needle]wor[needle]ld" `shouldBe` Just 1

    it "works in the beginning of a string" $ do
        f "[needle]" "[needle]helloworld" `shouldBe` Just 0

    it "works in the end of a string" $ do
        f "[needle]" "helloworld[needle]" `shouldBe` Just 10

    it "works on selfintersecting needles" $ do
        f "[needle[needle" "hello[needleworld[needle[needlestuff" `shouldBe` Just 17

    it "works on needles consisting of the same element" $ do
        f "aaaa" "a|aa|aaa|aaaa|aaaaa" `shouldBe` Just 9

    it "finds empty needles" $
        property $ \as -> f [] (as :: [Bool]) == Just 0

    it "doesn't find anything in empty haystacks" $
        property $ \as -> not (null as) ==> f (as :: [Bool]) [] == Nothing

    context "works on lists of Bool" $ do
        it "finds the needle when it is in the haystack" $
            property $ \as bs cs -> (not $ Data.List.isInfixOf as (bs ++ init as)) ==> f (as :: [Bool]) (bs ++ as ++ cs) == Just (length bs)

        it "returns Nothing when the needle is not in the haystack" $
            property $ \as bs -> (not $ Data.List.isInfixOf as bs) ==> f (as :: [Bool]) bs == Nothing

indexBySpec :: (forall a. (a -> a -> Bool) -> [a] -> [a] -> Maybe Int) -> SpecWith ()
indexBySpec f = do
    context "mimics indexOf when applied to (==)" $ do
        indexOfSpec $ f (==)
    
    it "mimicks indexOf with a forgetful predicate" $
        property $ \as bs -> f ((==) `on` fst) (zip (as :: [Bool]) [1..]) (zip bs [1..]) == indexOf as bs

    it "only tests length if predicate is constantly true" $
        property $ \as bs -> f (const $ const True) (as :: [Bool]) bs == if length as <= length bs then Just 0 else Nothing

main :: IO ()
main = hspec $ do
    describe "prefixFun" $ do
        prefixFunSpec prefixFun

    describe "prefixFunBy" $ do
        prefixFunBySpec prefixFunBy

    describe "genericPrefixFun" $ do
        context "mimicks prefixFun in an Int context" $ do
            prefixFunSpec genericPrefixFun

        it "works with Integer" $ do
            genericPrefixFun "abacaba" `shouldBe` [0 :: Integer, 0, 1, 0, 1, 2, 3]

        it "works with Float" $ do
            genericPrefixFun "abacaba" `shouldBe` [0.0 :: Float, 0, 1, 0, 1, 2, 3]

        it "works with Double" $ do
            genericPrefixFun "abacaba" `shouldBe` [0.0 :: Double, 0, 1, 0, 1, 2, 3]

    describe "genericPrefixFunBy" $ do
        context "mimicks prefixFunBy in an Int context" $ do
            prefixFunBySpec genericPrefixFunBy

        it "works with Integer" $ do
            genericPrefixFunBy (==) "abacaba" `shouldBe` [0 :: Integer, 0, 1, 0, 1, 2, 3]

        it "works with Float" $ do
            genericPrefixFunBy (==) "abacaba" `shouldBe` [0 :: Float, 0, 1, 0, 1, 2, 3]

        it "works with Double" $ do
            genericPrefixFunBy (==) "abacaba" `shouldBe` [0 :: Double, 0, 1, 0, 1, 2, 3]

    describe "isInfixOf" $ do
        isInfixOfSpec isInfixOf

    describe "isIndexBy" $ do
        isInfixBySpec isInfixBy

    describe "indexOf" $ do
        indexOfSpec indexOf

    describe "indexBy" $ do
        indexBySpec indexBy

    describe "genericIndexOf" $ do
        context "mimicks indexOf in an Int context" $ do
            indexOfSpec genericIndexOf

        it "works with Integer" $ do
            genericIndexOf "[needle]" "hello[needle]world" `shouldBe` Just (5 :: Integer)

        it "works with Float" $ do
            genericIndexOf "[needle]" "hello[needle]world" `shouldBe` Just (5 :: Float)

        it "works with Double" $ do
            genericIndexOf "[needle]" "hello[needle]world" `shouldBe` Just (5 :: Double)
            
    describe "genericIndexBy" $ do
        context "mimicks indexBy in an Int context" $ do
            indexBySpec genericIndexBy

        it "works with Integer" $ do
            genericIndexBy (==) "[needle]" "hello[needle]world" `shouldBe` Just (5 :: Integer)

        it "works with Float" $ do
            genericIndexBy (==) "[needle]" "hello[needle]world" `shouldBe` Just (5 :: Float)

        it "works with Double" $ do
            genericIndexBy (==) "[needle]" "hello[needle]world" `shouldBe` Just (5 :: Double)
