{-# LANGUAGE OverloadedStrings #-}

module Test.ProtocolSpec (spec) where

import           Data.Text              (Text)
import           Data.Text.Encoding     (encodeUtf8)
import           Test.Hspec

import qualified Network.HESP.Exception as E
import qualified Network.HESP.Protocol  as P


spec :: Spec
spec = do
  simpleString
  bulkString

simpleString :: Spec
simpleString = describe "Simple String" $ do
  context "Constructing" $ do
    it "has invalid char '\\r' should return HasInvalidChar error" $ do
      P.mkSimpleString "hello\rworld" `shouldSatisfy` hasInvalidCharErrEi
    it "'\\\\r' should return be parsed successfully" $ do
      let resultStr = "Right (SimpleString \"hello\\\\rworld\")"
      show (P.mkSimpleString "hello\\rworld") `shouldBe` resultStr

  context "Serialization" $ do
    let source = "hello, world"
    let result = "+hello, world\r\n"
    it "simple serialize" $ do
      P.serialize (P.mkSimpleStringUnsafe source) `shouldBe` result
    it "simple deserialize" $ do
      P.deserializeOnly result `shouldBe` Right (P.mkSimpleStringUnsafe source)

bulkString :: Spec
bulkString = describe "Bulk String" $ do
  context "Serialization" $ do
    let source = "hello, world"
    let result = "$12\r\nhello, world\r\n"
    let sourceUtf8 = encodeUtf8 ("你好，世界" :: Text)
    let sourceBS = encodeUtf8 ("你好，世界" :: Text)
    let resultUtf8 = encodeUtf8 ("$15\r\n你好，世界\r\n" :: Text)
    it "simple serialize" $ do
      P.serialize (P.mkBulkString source) `shouldBe` result
    it "simple deserialize" $ do
      P.deserializeOnly result `shouldBe` Right (P.mkBulkString source)
    it "utf8 serialize" $ do
      P.serialize (P.mkBulkString sourceUtf8) `shouldBe` resultUtf8
    it "utf8 deserialize" $ do
      P.deserializeOnly resultUtf8 `shouldBe` Right (P.mkBulkString sourceBS)

-------------------------------------------------------------------------------

hasInvalidCharErrEi :: Either E.ProtocolException a -> Bool
hasInvalidCharErrEi (Left (E.HasInvalidChar _)) = True
hasInvalidCharErrEi _                           = False
