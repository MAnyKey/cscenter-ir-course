{-# LANGUAGE ScopedTypeVariables, NoMonomorphismRestriction #-}
module Main where

import System.IO
import System.Directory
import System.FilePath
import System.Environment

import Control.DeepSeq
import Control.Monad
import Control.Applicative
import qualified Data.Foldable as F
import Data.Monoid
import Data.Maybe

import Pipes

import Data.Char
import Data.Int
import Data.Word

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
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed.Mutable (IOVector)
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LTIO
import qualified Data.Text.Encoding as E
import qualified Data.Text.IO as TIO

import Text.HTML.TagSoup
import Text.HTML.TagSoup.Fast.Utf8Only hiding (parseTags)
--import NLP.Tokenize.Text hiding (tokenize)

import HunspellLemmer
import Tokenizer hiding (tokenize)

data PushVector a = PVec {-# UNPACK #-} !Int !(IOVector a)

unsafePushBack s v x = do
  MV.unsafeWrite v s x
  return $ PVec (s+1) v

vectorSize (PVec s _) = s

--pushBack :: Unbox a => PushVector a -> a -> IO (PushVector a)
pushBack pv@(PVec s v) x
  | MV.length v /= s = unsafePushBack s v x
  | otherwise     = do
    v' <- MV.unsafeGrow v (s `div` 2)
    unsafePushBack s v' x

newVector :: MV.Unbox a => IO (PushVector a)
newVector = PVec 0 <$> MV.new initialSize
  where initialSize = 10

tokenize :: LT.Text -> [LT.Text]
tokenize = run (whitespace >=> uris >=> punctuation >=> contractions)

preprocess :: (Text -> [Text]) -> ByteString -> [[Text]]
preprocess stemmer = map (stemmer . T.toLower) . filterPunctuation . map LT.toStrict . tokenize . extractPlainText
  where extractPlainText = myInnerText . myparse
        myparse = {-# SCC "myparse" #-} parseTagsT
        filterPunctuation = filter (not . T.all isPunctuation)
        myInnerText = {-# SCC "inner-text" #-} myTextConcat . myInnerTextTags
          where
            myInnerTextTags = mapMaybe maybeTagText
            myTextConcat = LT.fromChunks

extractAllTokens :: Hunspell -> FilePath -> IO (FilePath, [(TokenIndex, Text)])
extractAllTokens hunspell filePath = do
  allTokens <- concat . addTokIndices . preprocess stemmer <$> BS.readFile filePath
  return (filePath, allTokens)
  where
    stemmer = stemText hunspell
    addTokIndices = zipWith zipF [1..]
    zipF = \idx toks -> zip (repeat idx) toks

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

createDefaultHunspell = makeHunspell "./hunspell-dictionaries/ru_RU.aff" "./hunspell-dictionaries/ru_RU.dic"

type DocumentId = Int
type Document = FilePath
type TokenIndex = Word32
type Postings = PushVector (DocumentId, TokenIndex)

type Index' = BasicHashTable Text Postings
type DocTable = BasicHashTable DocumentId Document

invert :: (Foldable t) => Index' -> DocumentId -> t (TokenIndex, Text) -> IO ()
invert hashtable document = F.mapM_ insert
  where insert (idx, token) = do
          val <- Hash.lookup hashtable token
          pv <- maybe newVector return val
          pv' <- pushBack pv (document, idx)
          Hash.insert hashtable token pv'

printPosting (tok, docs) = (putText tok) >> putStr " " >> print docs

putText = BS.putStr . E.encodeUtf8

createId :: Document -> IORef DocumentId -> DocTable -> IO DocumentId
createId doc docIdRef docsTable = do
  docId <- readIORef docIdRef
  Hash.insert docsTable docId doc
  writeIORef docIdRef (docId + 1)
  return docId

int64 = int64LE . fromIntegral
int32 = int32LE . fromIntegral

go ref f acc item = do
  modifyIORef ref (+1)
  f acc item

serializeDocTable :: DocTable -> IO Builder
serializeDocTable doctable = do
  ref <- newIORef 0
  docs <- Hash.foldM (go ref update) mempty doctable
  count <- readIORef ref
  return $ int64LE count <> docs
  where
    update builder (docid, doc) = return $
      let bytes = E.encodeUtf8 . T.pack $ doc
          bytesCount = BS.length bytes
      in (builder <> int64 docid <> int64 bytesCount <> byteString bytes)

serializePostings :: Postings -> IO Builder
serializePostings (PVec size v) = do
  freezedVector <- V.unsafeFreeze v
  let actualValues = V.slice 0 size freezedVector
      f = \(d, idx) b -> int32 d <> int32 idx <> b
  return $ V.foldr f mempty actualValues

serializeIndex :: Index' -> IO Builder
serializeIndex index = do
  ref <- newIORef 0
  postings <- Hash.foldM (go ref update) mempty index
  count <- readIORef ref
  return $ int64LE count <> postings
  where
    update builder (tok, docs) = do
      postings <- serializePostings $ docs
      let tokBytes = E.encodeUtf8 tok
          tokBytesCount = BS.length tokBytes
          postingsLength = vectorSize docs
      return (builder <> int64 tokBytesCount <> byteString tokBytes <> int64 postingsLength <> postings)

serialize :: DocTable -> Index' -> IO Builder
serialize doctable index = mappend <$> serializeDocTable doctable <*> serializeIndex index

-- testTables :: IO (DocTable, Index')
-- testTables = do
--   docs <- Hash.new
--   index <- Hash.new
--   Hash.insert docs 5 "foobar"
--   Hash.insert docs 1 "barfoo"
--   Hash.insert index (T.pack "hello") [5, 1]
--   Hash.insert index (T.pack "word") [5]
--   Hash.insert index (T.pack "world") [1]
--   return (docs, index)

dump indexName (docsTable, index) = do
  serialized <- toLazyByteString <$> serialize docsTable index
  L.writeFile indexName serialized

main = do
  [dir, indexName] <- getArgs
  Just hunspell <- createDefaultHunspell
  index <- Hash.new
  docIdRef <- newIORef 0
  docsTable <- Hash.new
  runEffect $ for (getRecursiveContents dir) $ \file -> lift $ do
    putStrLn $ "processing " ++ file
    (doc, tokens) <- extractAllTokens hunspell file
    docId <- createId doc docIdRef docsTable
    invert index docId tokens
  serialized <- toLazyByteString <$> serialize docsTable index
  let hash = encode . md5 $ serialized
  L.writeFile indexName serialized
  L.writeFile (addExtension indexName "md5") hash
