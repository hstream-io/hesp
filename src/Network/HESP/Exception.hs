module Network.HESP.Exception
  ( ProtocolException (..)
  ) where

import           Control.Exception (Exception)
import           Data.Typeable     (Typeable)

data ProtocolException = HasInvalidChar !String
  deriving (Typeable, Show, Eq)

instance Exception ProtocolException
