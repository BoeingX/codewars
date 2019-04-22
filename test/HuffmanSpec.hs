module HuffmanSpec where

import qualified Data.Map as M

import Test.Hspec
import Huffman

-- Codewars wraps HSpec: http://hspec.github.io/
spec :: Spec
spec = do
    describe "test frequencies" $ do
        it "aaabcc" $ do
            frequencies "aabccaa" `shouldBe` [('a', 4), ('b', 1), ('c', 2)]

    describe "tree construction" $ do
        it "handles empty list" $ do
            fromFrequencies ([] :: [(Char, Int)]) `shouldBe` Nothing
        it "handles singleton" $ do
            fromFrequencies (frequencies "a") `shouldBe` Nothing
        it "handles singleton2" $ do
            fromFrequencies (frequencies "aa") `shouldBe` Nothing
        it "handles general case" $ do
            fromFrequencies (frequencies "aabccaa") `shouldBe` Just (Node 7 (Node 3 (Leaf 1 'b') (Leaf 2 'c')) (Leaf 4 'a'))
        it "handles general case2" $ do
            fromFrequencies (frequencies "aaaabcc") `shouldBe` Just (Node 7 (Node 3 (Leaf 1 'b') (Leaf 2 'c')) (Leaf 4 'a'))
        it "handles general case3" $ do
            fromFrequencies (frequencies "7D,") `shouldBe` Just (Node 3 (Leaf 1 'D') (Node 2 (Leaf 1 ',') (Leaf 1 '7')))

    describe "toEncodeMap" $ do
        it "aaaabcc" $ do
            toEncodeMap <$> fromFrequencies (frequencies "aaaabcc") `shouldBe` Just (M.fromList [('a', [O]), ('b', [Z, Z]), ('c', [Z, O])])
        it "7D," $ do
            toEncodeMap <$> fromFrequencies (frequencies "7D,") `shouldBe` Just (M.fromList [('D', [Z]), (',', [O, Z]), ('7', [O, O])])

    describe "basic tests" $ let fs = frequencies "aaaabcc" in do
        it "aaaabcc encoded should have length 10" $
            fmap length (encode fs "aaaabcc") `shouldBe` Just 10
        it "empty list encode" $
            encode fs [] `shouldBe` Just []
        it "empty list decode" $
            decode fs [] `shouldBe` Just []

    describe "7D," $ let fs = frequencies "7D," in do
        it "7D, encode" $ do
            encode fs "7D," `shouldBe` Just [O, O, Z, O, Z]
        it "7D, decode" $ do
            decode fs [O, O, Z, O, Z] `shouldBe` Just "7D,"
    
    describe "length" $ do
        it "equal lengths with same frequencies if alphabet size is a power of two" $
            let enc = encode [('a', 1), ('b', 1)]
            in mapM (fmap length) [enc "a", enc "b"] `shouldBe` Just [1, 1]
        it "smaller length for higher frequency, if size of alphabet is not power of two" $
            let enc = encode [('a', 1), ('b', 1), ('c', 2)]
            in mapM (fmap length) [enc "a", enc "b", enc "c"] `shouldBe` Just [2, 2, 1]

    describe "error handling" $ do
        it "empty frequencies encode 1" $ encode [] "abc" `shouldBe` Nothing
        it "empty frequencies encode 2" $ encode [] "" `shouldBe` Nothing
        it "singleton frequency encode 1" $ encode [('a', 1)] "a" `shouldBe` Nothing
        it "singleton frequency encode 2" $ encode [('a', 1)] "" `shouldBe` Nothing
        
        it "empty frequencies decode 1" $ (decode [] [Z, O] :: Maybe String) `shouldBe` Nothing
        it "empty frequencies decode 2" $ (decode [] [] :: Maybe String) `shouldBe` Nothing
        it "singleton frequency decode 1" $ decode [('a', 1)] [Z, O] `shouldBe` Nothing
        it "singleton frequency decode 2" $ decode [('a', 1)] [] `shouldBe` Nothing

    describe "identity test" $ do
        it "should work with 7D," $ let fs = [(',',1),('7',1),('D',1)] in do
            decode fs <$> (encode fs "7D,") `shouldBe` Just (Just "7D,")
