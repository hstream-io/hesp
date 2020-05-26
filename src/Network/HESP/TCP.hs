{-# LANGUAGE FlexibleContexts #-}

module Network.HESP.TCP
  ( recvMsgs
  , sendMsg
  , sendMsgs

    -- * Connection
  , createTcpConnectionPool
  , withTcpConnection
  , simpleCreateTcpConnPool

    -- * Re-exported from simple-network
  , TCP.HostPreference (..)
  , TCP.connect
  , TCP.serve
  ) where

import           Control.Monad.IO.Class        (MonadIO)
import           Control.Monad.Trans.Control   (MonadBaseControl)
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as LBS
import qualified Data.ByteString.Lazy.Internal as LBS
import           Data.Pool                     (Pool)
import qualified Data.Pool                     as Pool
import           Data.Time                     (NominalDiffTime)
import           Data.Vector                   (Vector)
import qualified Network.Simple.TCP            as TCP
import           Network.Socket                (SockAddr, Socket)
import qualified Network.Socket                as NS

import           Network.HESP.Protocol         (deserializeWithMaybe, serialize)
import qualified Network.HESP.Types            as T

-------------------------------------------------------------------------------

-- FIXME: more elegantly
recvMsgs :: MonadIO m => Socket -> Int -> m (Vector (Either String T.Message))
recvMsgs sock bytes = deserializeWithMaybe (TCP.recv sock bytes) Nothing

sendMsg :: MonadIO m => Socket -> T.Message -> m ()
sendMsg sock = TCP.send sock . serialize

sendMsgs :: (MonadIO m, Traversable t) => Socket -> t T.Message -> m ()
sendMsgs sock = TCP.sendLazy sock . fromChunks . fmap serialize

simpleCreateTcpConnPool :: NS.HostName
                        -> NS.ServiceName
                        -> IO (Pool (Socket, SockAddr))
simpleCreateTcpConnPool h p = createTcpConnectionPool h p 1 10 20

createTcpConnectionPool :: NS.HostName
                        -> NS.ServiceName
                        -> Int
                        -- ^ The number of stripes (distinct sub-pools) to
                        -- maintain. The smallest acceptable value is 1.
                        -> NominalDiffTime
                        -- ^ Amount of time for which an unused resource is kept
                        -- open. The smallest acceptable value is 0.5 seconds.
                        --
                        -- The elapsed time before destroying a resource may be
                        -- a little longer than requested, as the reaper thread
                        -- wakes at 1-second intervals.
                        -> Int
                        -- ^ Maximum number of resources to keep open per
                        -- stripe. The smallest acceptable value is 1.
                        --
                        -- Requests for resources will block if this limit is
                        -- reached on a single stripe, even if other stripes
                        -- have idle resources available.
                        -> IO (Pool (Socket, SockAddr))
createTcpConnectionPool host port =
  let r = TCP.connectSock host port
      close = TCP.closeSock . fst
   in Pool.createPool r close

withTcpConnection :: MonadBaseControl IO m
                  => Pool (Socket, SockAddr)
                  -> ((Socket, SockAddr) -> m a)
                  -> m a
withTcpConnection = Pool.withResource

-------------------------------------------------------------------------------

-- | /O(c)/ Convert a bunch of strict 'ByteString' into a lazy 'ByteString'.
fromChunks :: Foldable t => t BS.ByteString -> LBS.ByteString
fromChunks = foldr LBS.chunk LBS.Empty
