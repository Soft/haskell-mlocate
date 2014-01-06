
import Prelude hiding (readFile)
import System.Environment (getArgs)
import Data.Binary.Get (Get, runGet, getWord8, getWord16be, getWord32be, getWord64be, getLazyByteStringNul, skip)
import Data.Word (Word32, Word8)
import Data.ByteString.Lazy (ByteString, readFile)
import Data.Map (Map, empty)
import Control.Monad (when)
import Control.Applicative ((<$>), (<*>), (<*))

-- Offset, Path
type Entry = (Int, String)

uncompress :: [Entry] -> [String]
uncompress = uncompress' 0 ""
  where
    uncompress' offset' path' ((offset, path):rest) = let prefix = offset' + offset
                                                          entry = take prefix path' ++ path
                                                      in entry : uncompress' prefix entry rest
    uncompress' _ _ [] = []

dummy = [ (0,  "/usr/src")
        , (8,  "/cmd/aardvark.c")
        , (6,  "rmadillo.c") ]

data Header = Header
  { confSize :: Word32
  , version :: Word8
  , checkVisibility :: Bool
  , root :: ByteString }
  deriving (Show)

type Config = Map ByteString [ByteString]

data Database = Database
  { header :: Header
  , config :: Config
  , entries :: [Entry] }
  deriving (Show)


-- man: mlocate.db

readHeader :: Get Header
readHeader = do
  magic <- getWord64be
  when (magic /= mlocate_magic) $ fail "Incorrect magic number"
  Header <$> getWord32be <*> getWord8 <*> ((== 1) <$> getWord8) <* getWord16be <*> getLazyByteStringNul
  where
    mlocate_magic = 0x006D6C6F63617465 -- \0 m l o c a t e

readConfig :: Int -> Get Config
readConfig length = skip length >> (return $ empty)

readEntry :: Get Entry
readEntry = return undefined

readDatabase :: Get Database
readDatabase = do
  header <- readHeader
  Database header <$> (readConfig $ fromIntegral $ confSize header) <*> (return [])

main = do
  (file:_) <- getArgs
  contents <- readFile file
  print $ runGet readDatabase contents



