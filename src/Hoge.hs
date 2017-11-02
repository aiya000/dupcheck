module Hoge where

import Control.Monad (when)
import Control.Monad.ListT (ListT) -- List
import Control.Monad.IO.Class (liftIO) -- transformers
import Data.Binary (encode) -- binary
import Data.Digest.Pure.MD5 -- pureMD5
import Data.List.Class (repeat, takeWhile, foldlL) -- List
import System.IO (IOMode(ReadMode), openFile, hClose)
import qualified Data.ByteString as BI
import qualified Data.ByteString.Lazy as BS
import Prelude hiding (repeat, takeWhile)

-- hashFile :: FilePath -> IO BS.ByteString
-- hashFile = fmap (encode . md5Finalize) . foldlL md5Update md5InitialContext . strictReadFileChunks 1024

strictReadFileChunks :: Int -> FilePath -> ListT IO BS.ByteString
strictReadFileChunks chunkSize filename =
  takeWhile (not . BS.null) $ do
    handle <- liftIO $ openFile filename ReadMode
    repeat () -- this makes the lines below loop
    chunk <- liftIO $ BS.hGet handle chunkSize
    when (BS.null chunk) . liftIO $ hClose handle
    return chunk

