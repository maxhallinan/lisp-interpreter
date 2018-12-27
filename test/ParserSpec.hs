module ParserSpec (main) where

import Test.Hspec (describe, hspec, it, shouldBe)
import Test.QuickCheck
import qualified Parser

parse = Parser.parse "fake-filename"

main :: IO ()
main = hspec $ do
  describe "parse" $ do
    describe "comment" $ do
      it "ignores a comment" $ do
        parse "#t;foo" `shouldBe` Right (Parser.Bool True)
        
    describe "boolean" $ do
      it "parses a boolean" $ do
        parse "#t" `shouldBe` Right (Parser.Bool True)
        parse "#f" `shouldBe` Right (Parser.Bool False)

    describe "number" $ do
      it "parses a number" $ do
        parse "1" `shouldBe` Right (Parser.Number 1)
        parse "123" `shouldBe` Right (Parser.Number 123)

    describe "string" $ do
      it "parses a string" $ do
        parse "\"a\"" `shouldBe` Right (Parser.String "a")
        parse "\"abc\"" `shouldBe` Right (Parser.String "abc")
        parse "\"1abc\"" `shouldBe` Right (Parser.String "1abc")
        parse "\"ab1c\"" `shouldBe` Right (Parser.String "ab1c")

    describe "atom" $ do
      it "parses an atom" $ do
        parse "a" `shouldBe` Right (Parser.Atom "a")
        parse "abc" `shouldBe` Right (Parser.Atom "abc")

    describe "list" $ do
      describe "proper list" $ do
        it "parses with spaces" $ do
          parse "( a 1 )" `shouldBe` Right (Parser.ProperList [Parser.Atom "a", Parser.Number 1])

        it "parses without spaces" $ do
          parse "(a 1)" `shouldBe` Right (Parser.ProperList [Parser.Atom "a", Parser.Number 1])

      describe "improper list" $ do
        it "parses with spaces" $ do
          parse "( a . 1 )" `shouldBe` Right (Parser.ImproperList [Parser.Atom "a"] (Parser.Number 1))

        it "parses without spaces" $ do
          parse "(a . 1)" `shouldBe` Right (Parser.ImproperList [Parser.Atom "a"] (Parser.Number 1))

      describe "nested list" $ do
        it "parses a nested list" $ do
          parse "(\"abc\" (a . 1))" `shouldBe` Right (Parser.ProperList [Parser.String "abc", Parser.ImproperList [Parser.Atom "a"] (Parser.Number 1)])
