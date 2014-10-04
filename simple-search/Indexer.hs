{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import System.IO
import System.Directory
import System.FilePath
import System.Environment

import Control.Monad
import Control.Applicative
import qualified Data.Foldable as F

import Pipes

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.HashTable.Class (HashTable)
import Data.HashTable.IO (IOHashTable, BasicHashTable)
import qualified Data.HashTable.IO as Hash
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Vector.Mutable (MVector)
import qualified Data.Vector.Mutable as MV
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.Text.IO as TIO


import Text.XML.HXT.Core
import Text.HTML.TagSoup
import NLP.Tokenize.Text hiding (tokenize)

import HunspellLemmer

data Token = Token
           deriving (Ord, Eq)

preprocess stemmer = map stemmer . tokenize . extractPlainText
  where extractPlainText = innerText . parseTags
        tokenize = run (whitespace >=> uris >=> punctuation >=> contractions)

stemmer hunspell = stemText hunspell

processFile process filePath = do
  tokenVariants <- process <$> TIO.readFile filePath
  let allTokens = HashSet.fromList $ concat tokenVariants
  return (takeFileName filePath, allTokens)


extractAllTokens hunspell = processFile (preprocess (stemmer hunspell))

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

transform :: (Monad m) => (a -> m b) -> Pipe a b m ()
transform f = await >>= lift . f >>= yield

tokens :: Hunspell -> Pipe FilePath (Document, HashSet Text) IO ()
tokens hunspell = transform (extractAllTokens hunspell)

x |> f = f x

tokens' hunspell = \file -> file |> extractAllTokens hunspell |> lift >>= yield

createDefaultHunspell = makeHunspell "./hunspell-dictionaries/ru_RU.aff" "./hunspell-dictionaries/ru_RU.dic"


type Document = FilePath
type Index = HashMap Text [Document]

type Index' = BasicHashTable Text [Document]

invert :: (Foldable t, HashTable h) =>
          IOHashTable h Text [Document] ->
          Document ->
          t Text ->
          IO ()
invert hashtable document = F.mapM_ insert
  where insert token = do
          val <- Hash.lookup hashtable token
          case val of
            Just docs -> Hash.insert hashtable token (document:docs)
            Nothing -> Hash.insert hashtable token [document]

construct table = await >>= lift . invert table

main = do
  [dir] <- getArgs
  Just hunspell <- createDefaultHunspell
  return ()
  index :: Index' <- Hash.new
  runEffect $ for (getRecursiveContents ~> tokens' hunspell $ dir) (lift . uncurry (invert index))
  print index
