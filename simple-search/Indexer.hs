module Main where

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import System.IO
import System.Directory
import System.FilePath
import System.Environment
import Control.Monad
import Pipes
import Data.Set (Set)

data Token = Token
           deriving (Ord, Eq)

preprocess = BS.getContents

tokenize :: ByteString -> [ByteString]
tokenize = undefined

getRecursiveContents :: FilePath -> Producer FilePath IO ()
getRecursiveContents topPath = do
  names <- lift $ getDirectoryContents topPath
  let properNames = filter (`notElem` [".", ".."]) names
  forM_ properNames $ \name -> do
    let path = topPath </> name
    isDirectory <- lift $ doesDirectoryExist path
    if isDirectory
      then getRecursiveContents path
      else yield path

main = do
  [dir] <- getArgs
  runEffect $ for (getRecursiveContents dir) (lift . putStrLn)
