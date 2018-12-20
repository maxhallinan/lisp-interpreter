module ParserSpec (main) where

import Text.Megaparsec (parseMaybe)
import Test.Hspec (describe, hspec, it, shouldBe)
import Test.QuickCheck
import Parser

main :: IO ()
main = hspec $ do
  describe "Parser" $ do
    describe "boolean" $ do
      it "parses a boolean" $ do
        parseMaybe Parser.lispBool "#t" `shouldBe` Just (LispBool True)
        parseMaybe Parser.lispBool "#f" `shouldBe` Just (LispBool False)

    describe "number" $ do
      it "parses a number" $ do
        parseMaybe Parser.lispNumber "1" `shouldBe` Just (LispNumber 1)
        parseMaybe Parser.lispNumber "123" `shouldBe` Just (LispNumber 123)

    describe "string" $ do
      it "parses a string" $ do
        parseMaybe Parser.lispString "\"a\"" `shouldBe` Just (LispString "a")
        parseMaybe Parser.lispString "\"abc\"" `shouldBe` Just (LispString "abc")
        parseMaybe Parser.lispString "\"1abc\"" `shouldBe` Just (LispString "1abc")
        parseMaybe Parser.lispString "\"ab1c\"" `shouldBe` Just (LispString "ab1c")

    describe "atom" $ do
      it "parses a boolean" $ do
        parseMaybe Parser.lispAtom "#t" `shouldBe` Just (LispBool True)
        parseMaybe Parser.lispAtom "#f" `shouldBe` Just (LispBool False)

      it "parses an atom" $ do
        parseMaybe Parser.lispAtom "a" `shouldBe` Just (LispAtom "a")
        parseMaybe Parser.lispAtom "abc" `shouldBe` Just (LispAtom "abc")

    describe "list" $ do
      describe "proper list" $ do
        it "parses with spaces" $ do
          parseMaybe Parser.lispList "( a 1 )" `shouldBe` Just (LispProperList [LispAtom "a", LispNumber 1])

        it "parses without spaces" $ do
          parseMaybe Parser.lispList "(a 1)" `shouldBe` Just (LispProperList [LispAtom "a", LispNumber 1])

      describe "improper list" $ do
        it "parses with spaces" $ do
          parseMaybe Parser.lispList "( a . 1 )" `shouldBe` Just (LispImproperList [LispAtom "a"] (LispNumber 1))

        it "parses without spaces" $ do
          parseMaybe Parser.lispList "(a . 1)" `shouldBe` Just (LispImproperList [LispAtom "a"] (LispNumber 1))

      describe "nested list" $ do
          it "parses a nested list" $ do
            parseMaybe Parser.lispList "(\"abc\" (a . 1))" `shouldBe` Just (LispProperList [LispString "abc", LispImproperList [LispAtom "a"] (LispNumber 1)])
