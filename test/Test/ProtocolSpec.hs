{-# LANGUAGE OverloadedStrings #-}

module Test.ProtocolSpec (spec) where

import           Data.Text          (Text)
import           Data.Text.Encoding (encodeUtf8)
import           Test.Hspec

import qualified Network.HESP       as P

spec :: Spec
spec = do
  boolean
  simpleString
  bulkString
  simpleError

boolean :: Spec
boolean = describe "Boolean" $ do
  context "Serialization" $ do
    let tr = "#t\r\n"
    let fr = "#f\r\n"
    it "serialize: True" $ do
      P.serialize (P.mkBoolean True) `shouldBe` tr
    it "serialize: False" $ do
      P.serialize (P.mkBoolean False) `shouldBe` fr
    it "deserialize: True" $ do
      P.deserialize tr `shouldBe` Right (P.mkBoolean True)
    it "deserialize: False" $ do
      P.deserialize fr `shouldBe` Right (P.mkBoolean False)

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
      P.deserialize result `shouldBe` Right (P.mkSimpleStringUnsafe source)

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
      P.deserialize result `shouldBe` Right (P.mkBulkString source)
    it "utf8 serialize" $ do
      P.serialize (P.mkBulkString sourceUtf8) `shouldBe` resultUtf8
    it "utf8 deserialize" $ do
      P.deserialize resultUtf8 `shouldBe` Right (P.mkBulkString sourceBS)

simpleError :: Spec
simpleError = describe "Simple Error" $ do
  context "Serialization" $ do
    let source = "this is an error message"
    let result = "-ERR this is an error message\r\n"
    it "serialize: generic error type" $ do
      P.serialize (P.mkSimpleError "ERR" source) `shouldBe` result
    it "deserialize: generic error type" $ do
      let expect = Right (P.mkSimpleError "ERR" source)
      P.deserialize result `shouldBe` expect
    let customType = "SOMEERROR"
    let customSrc = "this is an error message"
    let customRst = "-SOMEERROR this is an error message\r\n"
    let customRstSp = "-SOMEERROR   this is an error message\r\n"
    it "serialize: user defined error type" $ do
      let src = P.mkSimpleError customType customSrc
      P.serialize src `shouldBe` customRst
    it "deserialize: user defined error type" $ do
      let srcStr = show $ P.deserialize customRstSp
      let eptStr = "Right (SimpleError \"SOMEERROR\" "
                <> "\"this is an error message\")"
      srcStr `shouldBe` eptStr

-------------------------------------------------------------------------------

hasInvalidCharErrEi :: Either P.ProtocolException a -> Bool
hasInvalidCharErrEi (Left (P.HasInvalidChar _)) = True
hasInvalidCharErrEi _                           = False
