{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}

module DupCheck
  ( Options(..)
  , getOptions
  , listDirectories
  , listDuplicates
  , md5sum
  ) where

import Control.Monad.ListT (ListT)
import Control.Monad.IO.Class (liftIO)
import Data.Binary (encode)
import qualified Data.ByteString as BI
import qualified Data.ByteString.Lazy as LBI
import Data.Digest.Pure.MD5 ( MD5Digest(..)
                            , md5
                            , md5Finalize
                            , md5InitialContext
                            , md5Update
                            )
import qualified Data.List.Class as LC
import Data.List.Unique (sortUniq)
import Data.Version (showVersion)
import Paths_dupcheck (version)
import System.Console.CmdArgs ( Data
                              , Typeable
                              , (&=)
                              , args
                              , cmdArgs
                              , summary
                              )
import System.IO ( IOMode(ReadMode)
                 , hClose
                 , openFile
                 )
import System.Directory ( doesDirectoryExist
                        , listDirectory
                        )

data Options = Options
  { dirs :: [FilePath]
  } deriving (Show, Data, Typeable)

options :: Options
options = Options
  { dirs = [] &= args
  } &= summary ("dupcheck " ++ showVersion version)

getOptions :: IO (Options, Maybe String)
getOptions = do
  ops <- cmdArgs options
  mErrorMessage <- checkOptions ops
  return (ops, mErrorMessage)
 where
  checkOptions :: Options -> IO (Maybe String)
  checkOptions ops | dirs ops == [] = return $ Just "Please specify target directories"
                   | otherwise = do
    bs <- mapM doesDirectoryExist $ dirs ops
    let isValid = and bs
    return $ if isValid then Nothing else Just "Specified arguments are not directory"

listDirectories :: [FilePath] -> IO [FilePath]
listDirectories directories = listFiles (sortUniq $ map removeFileSeparator directories) []
 where
  removeFileSeparator :: FilePath -> FilePath
  removeFileSeparator filePath | last filePath == '/' = init filePath
                               | otherwise = filePath
  listFiles :: [FilePath] -> [FilePath] -> IO [FilePath]
  listFiles [] files = return files
  listFiles (d:ds) files = do
    listedFilesOrDirectories <- listDirectory d
    listedFiles <- mapM listFiles' listedFilesOrDirectories
    listFiles ds $ files ++ concat listedFiles
   where
    listFiles' :: FilePath -> IO [FilePath]
    listFiles' filePath = do
      let path = d ++ "/" ++ filePath
      b <- doesDirectoryExist path
      if b then listDirectories [path] else return [path]

md5sum :: FilePath -> IO (MD5Digest, FilePath)
md5sum file = LBI.readFile file >>= (\contents -> do let d = md5 contents
                                                     print d
                                                     return (d, file))

-- hashFile :: FilePath -> IO LBI.ByteString
-- hashFile = fmap (encode . md5Finalize) . LC.foldlL md5Update md5InitialContext . strictReadFileChunks 1024

-- strictReadFileChunks :: Int -> FilePath -> ListT IO BI.ByteString
-- strictReadFileChunks chunkSize filename =
--   takeWhile (not . LBI.null) $ do
--     handle <- liftIO $ openFile filename ReadMode
--     repeat () -- this makes the lines below loop
--     chunk <- liftIO $ LBI.hGet handle chunkSize
--     when (LBI.null chunk) . liftIO $ hClose handle
--     return chunk

listDuplicates :: [(MD5Digest, FilePath)] -> [[FilePath]]
listDuplicates [] = []
listDuplicates pairs = listDups (head pairs) (tail pairs) []
 where
  listDups :: (MD5Digest, FilePath) -> [(MD5Digest, FilePath)] -> [[FilePath]] -> [[FilePath]]
  listDups _ [] dups = dups
  listDups (digest, file) list dups = listDups (head list) (tail list) $ if filtered == [] then dups else (file:filtered):dups
   where
    filtered :: [FilePath]
    filtered = map snd (filter (\(key, _) -> digest == key) list)

