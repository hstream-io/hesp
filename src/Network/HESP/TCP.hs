module Network.HESP.TCP
  ( TCP.HostPreference (..)
  , TCP.connect
  , TCP.serve
  , recvMsg
  , sendMsg
  , sendMsgs
  ) where

import           Control.Monad.IO.Class        (MonadIO)
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as LBS
import qualified Data.ByteString.Lazy.Internal as LBS
import qualified Network.Simple.TCP            as TCP
import           Network.Socket                (Socket)

import           Network.HESP.Protocol         (deserializeWithMaybe, serialize)
import qualified Network.HESP.Types            as T

-------------------------------------------------------------------------------

recvMsg :: MonadIO m => Socket -> Int -> m (Either String T.Message)
recvMsg sock bytes = deserializeWithMaybe (TCP.recv sock bytes) Nothing

sendMsg :: MonadIO m => Socket -> T.Message -> m ()
sendMsg sock = TCP.send sock . serialize

sendMsgs :: (MonadIO m, Traversable t) => Socket -> t T.Message -> m ()
sendMsgs sock = TCP.sendLazy sock . fromChunks . fmap serialize

-------------------------------------------------------------------------------

-- | /O(c)/ Convert a bunch of strict 'ByteString' into a lazy 'ByteString'.
fromChunks :: Foldable t => t BS.ByteString -> LBS.ByteString
fromChunks = foldr LBS.chunk LBS.Empty
