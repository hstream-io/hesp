{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE ViewPatterns      #-}

module Network.HESP.Types
  ( Message ( Boolean
            , Integer
            , MatchSimpleString
            , MatchBulkString
            , MatchSimpleError
            , MatchArray
            , MatchPush
            )
    -- * Construction
  , mkSimpleString
  , mkSimpleStringUnsafe
  , mkBulkString
  , mkSimpleError
  , mkArray
  , mkArrayFromList
  , mkPush
  , mkPushFromList

    -- * Exception
  , ProtocolException (..)

    -- * Helpers
  , pattern Empty
  , pattern (:<)
  ) where

import           Control.DeepSeq       (NFData)
import           Control.Exception     (Exception)
import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Char8 as BS
import           Data.Typeable         (Typeable)
import           Data.Vector           (Vector)
import qualified Data.Vector           as V
import           GHC.Generics          (Generic)

-------------------------------------------------------------------------------

-- | Message that are send to remote, or receive from remote.
data Message = SimpleString ByteString
             | BulkString ByteString
             | SimpleError ByteString ByteString
             | Boolean Bool
             | Integer Integer
             | Array (Vector Message)
             | Push ByteString (Vector Message)
  deriving (Eq, Show, Generic, NFData)

-- | Simple strings can not contain the @CR@ nor the @LF@ characters inside.
mkSimpleString :: ByteString -> Either ProtocolException Message
mkSimpleString bs =
  let hasInvalidChar = BS.elem '\r' bs || BS.elem '\n' bs
   in if hasInvalidChar
         then Left $ HasInvalidChar "\r or \n"
         else Right $ SimpleString bs

-- | Make simple string without invalid characters checking.
mkSimpleStringUnsafe :: ByteString -> Message
mkSimpleStringUnsafe = SimpleString

mkBulkString :: ByteString -> Message
mkBulkString = BulkString

mkSimpleError :: ByteString -- ^ error type
              -> ByteString -- ^ error message
              -> Message
mkSimpleError = SimpleError

mkArray :: Vector Message -> Message
mkArray = Array

mkArrayFromList :: [Message] -> Message
mkArrayFromList = Array . V.fromList

mkPush :: ByteString -> Vector Message -> Message
mkPush = Push

mkPushFromList :: ByteString -> [Message] -> Message
mkPushFromList ty = Push ty . V.fromList

-- FIXME: complete sigs seems only work for the same module
{-# COMPLETE MatchSimpleString
  , MatchBulkString
  , MatchSimpleError
  , MatchArray
  , Boolean
  , Integer
  #-}

pattern MatchSimpleString :: ByteString -> Message
pattern MatchSimpleString x <- SimpleString x

pattern MatchBulkString :: ByteString -> Message
pattern MatchBulkString x <- BulkString x

pattern MatchSimpleError :: ByteString -> ByteString -> Message
pattern MatchSimpleError tp mg <- SimpleError tp mg

pattern MatchArray :: Vector Message -> Message
pattern MatchArray x <- Array x

pattern MatchPush :: ByteString -> Vector Message -> Message
pattern MatchPush x y <- Push x y

-------------------------------------------------------------------------------

newtype ProtocolException = HasInvalidChar String
  deriving (Typeable, Show, Eq)

instance Exception ProtocolException

-------------------------------------------------------------------------------
-- Helpers

-- | Match an empty Vector.
pattern Empty :: Vector a
pattern Empty <- (uncons -> Nothing)

-- | Pattern match on Vector of Messages.
pattern (:<) :: Message -> Vector Message -> Vector Message
pattern hd :< tl <- (uncons -> Just (hd, tl))

uncons :: Vector a -> Maybe (a, Vector a)
uncons v = if V.null v then Nothing else Just (V.head v, V.tail v)
