{-# LANGUAGE RecordWildCards #-}

module Network.HESP.TCP
  ( runTCPServer
  , ServerSettings (..)
  , TCP.HostPreference (..)
  ) where

import qualified Network.Simple.TCP    as TCP
import qualified Network.Socket        as NS

import           Network.HESP.Protocol (deserializeWithMaybe, serialize)
import qualified Network.HESP.Protocol as HP

data ServerSettings =
  ServerSettings { hostPreference :: TCP.HostPreference
                 , serviceName    :: NS.ServiceName
                 , maxRecvBytes   :: Int
                 }

runTCPServer :: ServerSettings
             -> (HP.Message -> IO (Maybe HP.Message))
             -> (String -> IO ())
             -> IO ()
runTCPServer ServerSettings{..} succC failC =
  TCP.serve hostPreference serviceName $ \(socket, _) -> do
    result <- deserializeWithMaybe (TCP.recv socket maxRecvBytes) Nothing
    case result of
      Left err -> failC err
      Right r  -> succC r >>= maybe (return ()) (TCP.send socket . serialize)
