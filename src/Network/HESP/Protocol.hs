{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.HESP.Protocol
  ( -- * Message
    Message (..)
    -- ** Extra Constructors
  , mkSimpleStringSafe
  , mkSimpleErrorSafe
  , mkArrayFromList

    -- * Serialization
  , serialize
  , deserialize
  , deserializeWith
  , deserializeWithMaybe
  ) where

import           Control.DeepSeq        (NFData)
import           Data.ByteString        (ByteString)
import qualified Data.ByteString.Char8  as BS
import qualified Data.Text              as Text
import qualified Data.Text.Encoding     as Text
import           Data.Vector            (Vector)
import qualified Data.Vector            as V
import           GHC.Generics           (Generic)
import qualified Scanner                as P

import           Network.HESP.Exception (ProtocolException (..))

-------------------------------------------------------------------------------

data Message = SimpleString ByteString
             | BulkString ByteString
             | SimpleError ByteString ByteString
             | Boolean Bool
             | Array (Vector Message)
  deriving (Eq, Show, Generic, NFData)

mkSimpleStringSafe :: ByteString -> Either ProtocolException Message
mkSimpleStringSafe bs =
  let hasInvalidChar = BS.elem '\r' bs || BS.elem '\n' bs
   in if hasInvalidChar
         then Left $ HasInvalidChar "\r or \n"
         else Right $ SimpleString bs

mkSimpleErrorSafe :: ByteString
                  -- ^ error type, should in upper case,
                  -- the generic one is @ERR@
                  -> ByteString
                  -- ^ error message
                  -> Message
mkSimpleErrorSafe errtype = SimpleError (toUpper errtype)
  where
    toUpper = Text.encodeUtf8 . Text.toUpper . Text.strip . Text.decodeUtf8

mkArrayFromList :: [Message] -> Message
mkArrayFromList xs = Array $ V.fromList xs

-------------------------------------------------------------------------------

serialize :: Message -> ByteString
serialize (SimpleString bs) = serializeSimpleString bs
serialize (BulkString bs)   = serializeBulkString bs
serialize (SimpleError t m) = serializeSimpleError t m
serialize (Boolean b)       = serializeBoolean b
serialize (Array xs)        = serializeArray xs

-- | Deserialize the complete input, without resupplying.
deserialize :: ByteString -> Either String Message
deserialize = P.scanOnly parser

-- | Deserialize with the provided resupply action.
deserializeWith :: Monad m
                => m ByteString         -- ^ resupply action
                -> ByteString           -- ^ input
                -> m (Either String Message)
deserializeWith = flip runScanWith parser

deserializeWithMaybe :: Monad m
                     => m (Maybe ByteString)
                     -> Maybe ByteString
                     -> m (Either String Message)
deserializeWithMaybe = flip runScanWithMaybe parser

-------------------------------------------------------------------------------
-- Serialize

serializeSimpleString :: ByteString -> ByteString
serializeSimpleString bs = BS.cons '+' bs <> sep

serializeBulkString :: ByteString -> ByteString
serializeBulkString bs = BS.cons '$' $ len <> sep <> bs <> sep
  where len = pack $ BS.length bs

serializeSimpleError :: ByteString -> ByteString -> ByteString
serializeSimpleError errtype errmsg = BS.concat ["-", errtype, " ", errmsg, sep]

serializeBoolean :: Bool -> ByteString
serializeBoolean True  = BS.cons '#' $ "t" <> sep
serializeBoolean False = BS.cons '#' $ "f" <> sep

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
    '-' -> uncurry SimpleError <$> err
    '#' -> Boolean <$> bool
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

{-# INLINE bool #-}
bool :: P.Scanner Bool
bool = b <* eol
  where
    b = do
      c <- P.anyChar8
      case c of
        't' -> return True
        _   -> return False

{-# INLINE str #-}
str :: P.Scanner ByteString
str = P.takeWhileChar8 (/= '\r') <* eol

{-# INLINE fixedstr #-}
fixedstr :: P.Scanner ByteString
fixedstr = do
  len <- decimal
  P.take len <* eol

{-# INLINE err #-}
err :: P.Scanner (ByteString, ByteString)
err = do
  errtype <- word
  errmsg <- str
  return (errtype, errmsg)

{-# INLINE word #-}
word :: P.Scanner ByteString
word = P.takeWhileChar8 (/= ' ') <* P.skipSpace

{-# INLINE eol #-}
eol :: P.Scanner ()
eol = P.char8 '\r' *> P.char8 '\n'

runScanWith :: Monad m
            => m ByteString
            -> P.Scanner a
            -> ByteString
            -> m (Either String a)
runScanWith more s input = go input (P.scan s)
  where
    go bs next =
      case next bs of
        P.More next'    -> more >>= \bs' -> go bs' next'
        P.Done _ r      -> return $ Right r
        P.Fail _ errmsg -> return $ Left errmsg

runScanWithMaybe :: Monad m
                 => m (Maybe ByteString)
                 -> P.Scanner a
                 -> Maybe ByteString
                 -> m (Either String a)
runScanWithMaybe more s input = go input (P.scan s)
  where
    go Nothing next = more >>= \bs' -> go bs' next
    go (Just bs) next =
      case next bs of
        P.More next'    -> more >>= \bs' -> go bs' next'
        P.Done _ r      -> return $ Right r
        P.Fail _ errmsg -> return $ Left errmsg
