{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import System.IO
import System.Directory
import System.FilePath
import System.Environment

import Control.Monad
import Control.Applicative
import qualified Data.Foldable as F
import Data.Monoid

import Pipes


import Data.Char

import Data.IORef
import Data.Digest.Pure.MD5
import Data.Binary

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Lazy.Builder
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

tokenize = run (whitespace >=> uris >=> punctuation >=> contractions)

preprocess stemmer = map (stemmer . T.toLower) . filterPunctuation . tokenize . extractPlainText
  where extractPlainText = innerText . parseTags
        filterPunctuation = filter (not . T.all isPunctuation)


stemmer hunspell = stemText hunspell

processFile process filePath = do
  tokenVariants <- process <$> TIO.readFile filePath
  let allTokens = HashSet.fromList $ concat tokenVariants
  return (filePath, allTokens)


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

--transform :: (Monad m) => (a -> m b) -> Pipe a b m ()
--transform f = await >>= lift . f >>= yield

-- tokens :: Hunspell -> Pipe FilePath (Document, HashSet Text) IO ()
-- tokens hunspell = transform (extractAllTokens hunspell)

-- x |> f = f x

-- tokens' hunspell = \file -> file |> extractAllTokens hunspell |> lift >>= yield

createDefaultHunspell = makeHunspell "./hunspell-dictionaries/ru_RU.aff" "./hunspell-dictionaries/ru_RU.dic"


type DocumentId = Int
type Document = FilePath

type Index' = BasicHashTable Text [DocumentId]
type DocTable = BasicHashTable DocumentId Document

-- invert :: (Foldable t, HashTable h) =>
--           IOHashTable h Text [DocumentId] ->
--           DocumentId ->
--           t Text ->
--           IO ()
invert :: (Foldable t) => Index' -> DocumentId -> t Text -> IO ()
invert hashtable document = F.mapM_ insert
  where insert token = do
          val <- Hash.lookup hashtable token
          case val of
            Just docs -> Hash.insert hashtable token (document:docs)
            Nothing -> Hash.insert hashtable token [document]

printPosting (tok, docs) = (putText tok) >> putStr " " >> print docs

putText = BS.putStr . E.encodeUtf8

createId :: Document -> IORef DocumentId -> DocTable -> IO DocumentId
createId doc docIdRef docsTable = do
  docId <- readIORef docIdRef
  Hash.insert docsTable docId doc
  writeIORef docIdRef (docId + 1)
  return docId

int64 = int64LE . fromIntegral

go ref f acc item = do
  modifyIORef ref (+1)
  return $ f acc item

serializeDocTable :: DocTable -> IO Builder
serializeDocTable doctable = do
  ref <- newIORef 0
  docs <- Hash.foldM (go ref update) mempty doctable
  count <- readIORef ref
  return $ int64LE count <> docs
  where
    update builder (docid, doc) =
      let bytes = E.encodeUtf8 . T.pack $ doc
          bytesCount = BS.length bytes
      in (builder <> int64 docid <> int64 bytesCount <> byteString bytes)

serializePostings :: [DocumentId] -> Builder
serializePostings = foldr f mempty
  where f = \d b -> int64 d <> b

serializeIndex :: Index' -> IO Builder
serializeIndex index = do
  ref <- newIORef 0
  postings <- Hash.foldM (go ref update) mempty index
  count <- readIORef ref
  return $ int64LE count <> postings
  where
    update builder (tok, docs) =
      let tokBytes = E.encodeUtf8 tok
          tokBytesCount = BS.length tokBytes
          postings = serializePostings $ docs
          postingsLength = length docs
      in (builder <> int64 tokBytesCount <> byteString tokBytes <> int64 postingsLength <> postings)

serialize :: DocTable -> Index' -> IO Builder
serialize doctable index = mappend <$> serializeDocTable doctable <*> serializeIndex index

testTables :: IO (DocTable, Index')
testTables = do
  docs <- Hash.new
  index <- Hash.new
  Hash.insert docs 5 "foobar"
  Hash.insert docs 1 "barfoo"
  return (docs, index)

main = do
  [dir, indexName] <- getArgs
  Just hunspell <- createDefaultHunspell
  index <- Hash.new
  docIdRef <- newIORef 0
  docsTable <- Hash.new
  runEffect $ for (getRecursiveContents dir) $ \file -> lift $ do
    (doc, tokens) <- extractAllTokens hunspell file
    docId <- createId doc docIdRef docsTable
    invert index docId tokens
  serialized <- toLazyByteString <$> serialize docsTable index
  let hash = encode . md5 $ serialized
  L.writeFile indexName serialized
  L.writeFile (addExtension indexName "md5") hash
