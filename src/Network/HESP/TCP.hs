{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Network.HESP.TCP
  ( runTCPServer
  , runTCPServerG
  , runTCPServer'
  , runTCPServerG'
  , open
  , close
  , setDefaultSocketOptions

  , recvMsgs
  , sendMsg
  , sendMsgs

    -- * Connection
  , createTcpConnectionPool
  , withTcpConnection
  , simpleCreateTcpConnPool

    -- * Re-exported from network-simple
  , TCP.HostPreference (..)
  , TCP.connect
  ) where

import           Control.Concurrent            (forkFinally)
import qualified Control.Concurrent.Lifted     as L
import           Control.Exception             (bracket)
import           Control.Monad                 (forever, void)
import           Control.Monad.IO.Class        (MonadIO, liftIO)
import           Control.Monad.Trans.Control   (MonadBaseControl, liftBaseOp)
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

runTCPServer :: NS.HostName
             -> NS.ServiceName
             -> ((Socket, SockAddr) -> IO a)
             -> IO b
runTCPServer host port =
  runTCPServer' host port setDefaultSocketOptions gracefulClose

runTCPServer' :: NS.HostName
              -> NS.ServiceName
              -> (Socket -> IO ()) -- ^ set socket options
              -> (Socket -> IO ()) -- ^ computation to run if exception happens
              -> ((Socket, SockAddr) -> IO a)
              -> IO b
runTCPServer' host port setOpts release server = do
  addr <- resolve host port
  bracket (open addr setOpts) close (acceptConc release server)

-- | Generalized version of 'runTCPServer'.
runTCPServerG :: (MonadBaseControl IO m, MonadIO m)
              => NS.HostName
              -> NS.ServiceName
              -> ((Socket, SockAddr) -> m ())
              -> m a
runTCPServerG host port =
  let release sock = liftIO $ gracefulClose sock
   in runTCPServerG' host port setDefaultSocketOptions release

runTCPServerG' :: (MonadBaseControl IO m, MonadIO m)
               => NS.HostName
               -> NS.ServiceName
               -> (Socket -> IO ())
               -- ^ set socket options
               -> (Socket -> m ())
               -- ^ computation to run if exception happens
               -> ((Socket, SockAddr) -> m ())
               -> m a
runTCPServerG' host port setOpts release server = do
  addr <- liftIO $ resolve host port
  gbracket (open addr setOpts) close (acceptConc' release server)

-- FIXME: more elegantly
recvMsgs :: MonadIO m => Socket -> Int -> m (Vector (Either String T.Message))
recvMsgs sock bytes = deserializeWithMaybe (TCP.recv sock bytes) ""

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
   in Pool.createPool r (close . fst)

withTcpConnection :: MonadBaseControl IO m
                  => Pool (Socket, SockAddr)
                  -> ((Socket, SockAddr) -> m a)
                  -> m a
withTcpConnection = Pool.withResource

-------------------------------------------------------------------------------

acceptConc :: (Socket -> IO ())
           -> ((Socket, SockAddr) -> IO a)
           -- ^ Computation to run when exception happends.
           -> Socket
           -> IO b
acceptConc release server sock = forever $ do
  (conn, peer) <- NS.accept sock
  void $ forkFinally (server (conn, peer)) (const $ release conn)

-- | Generalized version of 'acceptConc'.
acceptConc' :: (MonadBaseControl IO m, MonadIO m)
            => (Socket -> m ())
            -- ^ Computation to run when exception happends.
            -> ((Socket, SockAddr) -> m ())
            -> Socket
            -> m a
acceptConc' release server sock = forever $ do
  (conn, peer) <- liftIO $ NS.accept sock
  void $ L.forkFinally (server (conn, peer)) (const . release $ conn)

resolve :: NS.HostName -> NS.ServiceName -> IO NS.AddrInfo
resolve host port =
  let hints = NS.defaultHints { NS.addrFlags = [NS.AI_PASSIVE]
                              , NS.addrSocketType = NS.Stream
                              }
   in head <$> NS.getAddrInfo (Just hints) (Just host) (Just port)

open :: NS.AddrInfo -> (Socket -> IO ()) -> IO Socket
open NS.AddrInfo{..} setSocketOption = do
  sock <- NS.socket addrFamily addrSocketType addrProtocol
  setSocketOption sock
  NS.bind sock addrAddress
  NS.listen sock (max 2048 NS.maxListenQueue)
  return sock

-- | Shuts down and closes the Socket, silently ignoring any synchronous
-- exception that might happen.
close :: Socket -> IO ()
close = TCP.closeSock

gracefulClose :: Socket -> IO ()
gracefulClose conn = NS.gracefulClose conn 5000

setDefaultSocketOptions :: Socket -> IO ()
setDefaultSocketOptions sock = do
  NS.setSocketOption sock NS.ReuseAddr 1
  NS.setSocketOption sock NS.NoDelay 1
  NS.setSocketOption sock NS.ReuseAddr 1
  NS.setSocketOption sock NS.KeepAlive 1
  NS.withFdSocket sock NS.setCloseOnExecIfNeeded

-------------------------------------------------------------------------------

{-# INLINABLE fromChunks #-}
-- | /O(c)/ Convert a bunch of strict 'ByteString' into a lazy 'ByteString'.
fromChunks :: Foldable t => t BS.ByteString -> LBS.ByteString
fromChunks = foldr LBS.chunk LBS.Empty

{-# INLINABLE gbracket #-}
gbracket :: MonadBaseControl IO m => IO a -> (a -> IO b) -> (a -> m c) -> m c
gbracket acquire release = liftBaseOp (bracket acquire release)
