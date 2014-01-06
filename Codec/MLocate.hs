module Codec.MLocate (Header(..), Directory(..), Entry(..), Database(..), Config, readDatabase) where

import Prelude hiding (readFile, null)
import System.Environment (getArgs)
import Data.Binary.Get (Get, runGet, getWord8, getWord16be, getWord32be, getWord64be, getLazyByteStringNul)
import Data.Word (Word8, Word32, Word64)
import Data.ByteString.Lazy (ByteString, null, readFile)
import Data.Map (Map, fromList)
import Control.Monad (when)
import Control.Applicative ((<$>), (<*>), (<*), many, some, empty)

data Header = Header
  { confSize :: Word32
  , version :: Word8
  , checkVisibility :: Bool
  , root :: ByteString }
  deriving (Show)

type Config = Map ByteString [ByteString]

data Directory = Directory
  { timeS :: Word64
  , timeNS :: Word32
  , path :: ByteString
  , contents :: [Entry] }
  deriving (Show)

data Entry = File ByteString | SubDirectory ByteString
  deriving (Show)

data Database = Database
  { header :: Header
  , config :: Config
  , entries :: [Directory] }
  deriving (Show)

-- man: mlocate.db

getHeader :: Get Header
getHeader = do
  magic <- getWord64be
  when (magic /= mlocate_magic) $ fail "Incorrect magic number"
  Header <$> getWord32be <*> getWord8 <*> ((== 1) <$> getWord8) <* getWord16be <*> getLazyByteStringNul
  where
    mlocate_magic = 0x006D6C6F63617465 -- \0 m l o c a t e

getConfig :: Get Config
getConfig = fromList <$> many getAssignment
  where
  getAssignment = (,) <$> getLazyByteStringNul <*> some nonEmptyString <* getWord8
  nonEmptyString = do
    val <- getLazyByteStringNul
    if null val then empty else return val

getDirectory :: Get Directory
getDirectory = Directory <$> getWord64be <*> getWord32be <* getWord32be <*> getLazyByteStringNul <*> many getEntry <* getWord8

getEntry :: Get Entry
getEntry = do
  entryType <- getWord8
  case entryType of
    0 -> File <$> getLazyByteStringNul
    1 -> SubDirectory <$> getLazyByteStringNul
    _ -> empty

getDatabase :: Get Database
getDatabase = do
  Database <$> getHeader <*> getConfig <*> many getDirectory

readDatabase :: ByteString -> Database
readDatabase = runGet getDatabase

main = do
  (file:_) <- getArgs
  contents <- readFile file
  print $ readDatabase contents



