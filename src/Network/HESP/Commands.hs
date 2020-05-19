{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Network.HESP.Commands
  ( CommandName
  , CommandParams
  , CommandAction
  , Commands
  , mkCommandsFromList
  , commandRegister
  , getCommand
  , runCommand
  , commandParser
  , extractBulkStringParam
  , extractBulkStringParam2
  ) where

import           Control.Applicative (liftA2)
import           Data.ByteString     (ByteString)
import           Data.Map.Strict     (Map)
import qualified Data.Map.Strict     as Map
import           Data.Maybe          (fromMaybe)
import           Data.Vector         (Vector, (!?))
import qualified Data.Vector         as V

import           Network.HESP.Types  (Message (..))

-------------------------------------------------------------------------------

type CommandName = ByteString
type CommandParams = Vector Message
type CommandAction = CommandParams -> IO ()

newtype Commands = Commands (Map CommandName CommandAction)
  deriving (Semigroup, Monoid)

mkCommandsFromList :: [(CommandName, CommandAction)] -> Commands
mkCommandsFromList = Commands . Map.fromList

commandRegister :: CommandName
                -> (CommandParams -> IO ())
                -> Commands
                -> Commands
commandRegister name action (Commands cmds) =
  Commands $ Map.insert name action cmds

getCommand :: Commands -> CommandName -> Maybe CommandAction
getCommand (Commands cmds) name = Map.lookup name cmds

runCommand :: Commands -> CommandName -> CommandParams -> IO ()
runCommand cmds name params =
  let action = getCommand cmds name
      f = fromMaybe (const $ return ()) action
   in f params

commandParser :: Message -> Either ByteString (CommandName, CommandParams)
commandParser msg = validateProtoType msg >>= validateCommand

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
