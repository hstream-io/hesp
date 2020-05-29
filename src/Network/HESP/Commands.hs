{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Network.HESP.Commands
  ( CommandName
  , CommandParams
  , CommandAction (CommandAction)
  , CommandBox
  , mkCommandsFromList
  , commandRegister
  , getCommand
  , commandParser
  , extractBulkStringParam
  , extractBulkStringParam2
  , getBulkStringParam
  ) where

import           Control.Applicative (liftA2)
import           Data.ByteString     (ByteString)
import           Data.Map.Strict     (Map)
import qualified Data.Map.Strict     as Map
import           Data.Vector         (Vector, (!?))
import qualified Data.Vector         as V

import           Network.HESP.Types  (Message (..))
import qualified Network.HESP.Types  as T

-------------------------------------------------------------------------------

type CommandName = ByteString
type CommandParams = Vector Message

newtype CommandAction a = CommandAction (CommandParams -> a)

instance Show (CommandAction a) where
  show _ = "<CommandAction>"

newtype CommandBox a = CommandBox (Map CommandName (CommandAction a))
  deriving (Semigroup, Monoid, Show)

mkCommandsFromList :: [(CommandName, CommandAction a)] -> CommandBox a
mkCommandsFromList = CommandBox . Map.fromList

commandRegister :: CommandName -> CommandAction a -> CommandBox a -> CommandBox a
commandRegister name action (CommandBox cmds) =
  CommandBox $ Map.insert name action cmds

getCommand :: CommandBox a -> CommandName -> Maybe (CommandAction a)
getCommand (CommandBox cmds) name = Map.lookup name cmds

commandParser :: Message -> Either ByteString (CommandName, CommandParams)
commandParser msg = validateProtoType msg >>= validateCommand

getBulkStringParam :: CommandParams -> Int -> Maybe ByteString
getBulkStringParam params idx = T.getBulkString =<< (params !? idx)

extractBulkStringParam :: ByteString      -- ^ label
                       -> CommandParams   -- ^ vector of params
                       -> Int             -- ^ index
                       -> Either ByteString ByteString
                       -- ^ Either error message or bulk string
extractBulkStringParam label params idx =
  case params !? idx of
    Just (MatchBulkString x) -> Right x
    Just _                   -> Left $ label <> " must be a bulk string."
    Nothing                  -> Left $ label <> " can not be empty."

extractBulkStringParam2 :: (ByteString, Int)
                        -> (ByteString, Int)
                        -> CommandParams
                        -> Either ByteString (ByteString, ByteString)
extractBulkStringParam2 (l, i) (l', i') params =
  let r = extractBulkStringParam l params i
      r' = extractBulkStringParam l' params i'
   in liftA2 (,) r r'

-------------------------------------------------------------------------------

validateProtoType :: Message -> Either ByteString (Vector Message)
validateProtoType (MatchArray ms) = Right ms
validateProtoType _ = Left "Command must be sent through array type."

validateCommand :: Vector Message
                -> Either ByteString (CommandName, CommandParams)
validateCommand ms =
  let name = extractBulkStringParam "Command name" ms 0
      -- an empty vector is returned if @ms@ is empty, there is no exception.
      payloads = V.drop 1 ms
   in liftA2 (,) name (Right payloads)
