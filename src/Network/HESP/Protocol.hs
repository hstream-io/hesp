{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.HESP.Protocol
  ( Message
    -- ** Constructors
  , mkSimpleString
  , mkSimpleStringUnsafe
  , mkBulkString
  , mkArray
  , mkArrayFromList

    -- * Serialization
  , serialize
  , deserializeOnly
  ) where

import           Control.DeepSeq        (NFData)
import           Data.ByteString        (ByteString)
import qualified Data.ByteString.Char8  as BS
import           Data.Vector            (Vector)
import qualified Data.Vector            as V
import           GHC.Generics           (Generic)
import qualified Scanner                as P

import           Network.HESP.Exception (ProtocolException (..))

-------------------------------------------------------------------------------

data Message = SimpleString ByteString
             | BulkString ByteString
             | Array (Vector Message)
  deriving (Eq, Show, Generic, NFData)

mkSimpleString :: ByteString -> Either ProtocolException Message
mkSimpleString bs =
  let hasInvalidChar = BS.elem '\r' bs || BS.elem '\n' bs
   in if hasInvalidChar
         then Left $ HasInvalidChar "\r or \n"
         else Right $ SimpleString bs

mkSimpleStringUnsafe :: ByteString -> Message
mkSimpleStringUnsafe = SimpleString

mkBulkString :: ByteString -> Message
mkBulkString = BulkString

mkArray :: Vector Message -> Message
mkArray = Array

mkArrayFromList :: [Message] -> Message
mkArrayFromList xs = Array $ V.fromList xs

serialize :: Message -> ByteString
serialize (SimpleString bs) = serializeSimpleString bs
serialize (BulkString bs)   = serializeBulkString bs
serialize (Array xs)        = serializeArray xs

deserializeOnly :: ByteString -> Either String Message
deserializeOnly = P.scanOnly parser

-------------------------------------------------------------------------------
-- Serialize

serializeSimpleString :: ByteString -> ByteString
serializeSimpleString bs = BS.cons '+' bs <> sep

serializeBulkString :: ByteString -> ByteString
serializeBulkString bs = BS.cons '$' $ len <> sep <> bs <> sep
  where len = pack $ BS.length bs

serializeArray :: Vector Message -> ByteString
serializeArray ms = BS.cons '*' $ len <> sep <> go ms
  where
    len = pack $ V.length ms
    go xs = if V.null xs
               then ""
               else serialize (V.head xs) <> go (V.tail xs)

sep :: ByteString
sep = "\r\n"

pack :: (Show a) => a -> ByteString
pack = BS.pack . show

-------------------------------------------------------------------------------
-- Deserialize

parser :: P.Scanner Message
parser = do
  c <- P.anyChar8
  case c of
    '+' -> SimpleString <$> str
    '$' -> BulkString <$> fixedstr
    '*' -> Array <$> array
    _   -> fail $ BS.unpack $ "Unknown type: " `BS.snoc` c

{-# INLINE array #-}
array :: P.Scanner (V.Vector Message)
array = do
  len <- decimal
  V.replicateM len parser

-- | Parse a non-negative decimal number in ASCII. For example, @10\r\n@
{-# INLINE decimal #-}
decimal :: Integral n => P.Scanner n
decimal = P.decimal <* eol

{-# INLINE str #-}
str :: P.Scanner ByteString
str = P.takeWhileChar8 (/= '\r') <* eol

{-# INLINE fixedstr #-}
fixedstr :: P.Scanner ByteString
fixedstr = do
  len <- decimal
  P.take len <* eol

{-# INLINE eol #-}
eol :: P.Scanner ()
eol = P.char8 '\r' *> P.char8 '\n'
