module ParserSpec (run) where

import Test.Hspec (describe, hspec, it, shouldBe)
import qualified Parser as P
import qualified LispVal as L

run :: IO ()
run = hspec $ describe "src/Parser.hs" $ do
  describe "readSexpr" $ do

    describe "boolean" $ do
      it "parses true" $
        P.readSexpr "true" `shouldBe` (Right $ L.Bol True)
      it "parses false" $
        P.readSexpr "false" `shouldBe` (Right $ L.Bol False)

    describe "integer" $ do
      it "parses an unsigned integer" $
        P.readSexpr "1" `shouldBe` (Right $ L.Int 1)
      it "parses a signed integer" $
        P.readSexpr "-1" `shouldBe` (Right $ L.Int $ negate 1)

    describe "list" $ do
      it "parses an unquoted list" $ do
        P.readSexpr "(foo \"a\" 1)" `shouldBe` (Right $ L.List [L.Symbol "foo", L.Str "a", L.Int 1])

      it "parses a quoted list" $
        P.readSexpr "'(a b c)" `shouldBe` (Right $ L.List [L.Symbol "quote", L.List [L.Symbol "a", L.Symbol "b", L.Symbol "c"]])

    describe "nil" $ do
      it "parses the nil value" $
        P.readSexpr "()" `shouldBe` (Right $ L.Nil)

    -- describe "s-expression" $ do
    -- describe "function" $ do

    describe "string" $ do
      it "parses a string" $
        P.readSexpr "\"a\"" `shouldBe` (Right $ L.Str "a")

    describe "symbol" $ do
      it "parses a symbol" $
        P.readSexpr "a" `shouldBe` (Right $ L.Symbol "a")
