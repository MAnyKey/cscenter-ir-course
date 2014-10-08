{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
module Searcher where

import Prelude hiding (take)
import qualified Prelude as Pr

import System.IO
import System.Directory
import System.FilePath
import System.Environment

import Control.Arrow
import Control.Monad
import Control.Monad.Identity
import Control.Applicative
import qualified Data.Foldable as F
import Data.Monoid

import Pipes

import Data.Char
import Data.Int
import Data.Word
import Data.List (intercalate)
import Data.Bits

import Data.IORef
import Data.Binary
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Lazy.Builder
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Vector.Mutable (MVector)
import qualified Data.Vector.Mutable as MV
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.Text.IO as TIO

import Data.Attoparsec.ByteString.Lazy
--import qualified Data.Attoparsec.Text as AttT

import qualified Text.Parsec as P
import qualified Text.Parsec.Text as PT

import Crypto.Hash.MD5

import HunspellLemmer

type Document = Text
type DocumentId = Int64
type Token = Text

type Docs = HashMap DocumentId Document
type Index = HashMap Text (Vector DocumentId)
data Inv = Inv Docs Index
         deriving (Show, Eq)

infixl 7 *|*
(*|*) :: Word32 -> Word32 -> Word32
(*|*) b l = shiftL b 8 .|. l


makeUInt32 :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
makeUInt32 a b c d = result
  where result = d' *|* c' *|* b' *|* a'
        (a', (b', (c', d'))) = mapp fromIntegral (a, (b, (c, d)))
        mapp f  =  f *** f *** f *** f

makeUInt64 :: Word32 -> Word32 -> Word64
makeUInt64 lower higher = shiftL (fromIntegral higher) 32 .|. (fromIntegral lower)

anyUInt64LE = do
  lower <- makeUInt32 <$> anyWord8 <*> anyWord8 <*> anyWord8 <*> anyWord8
  higher <- makeUInt32 <$> anyWord8 <*> anyWord8 <*> anyWord8 <*> anyWord8
  return $ makeUInt64 lower higher

doc :: Parser (DocumentId, Document)
doc = do
  docId <- fromIntegral <$> anyUInt64LE
  bytesCount <- fromIntegral <$> anyUInt64LE
  bytes <- take bytesCount
  return $ (docId, E.decodeUtf8 bytes)

docs :: Parser Docs
docs = do
  size <- fromIntegral <$> anyUInt64LE
  docList <- count size doc
  return $ HashMap.fromList docList

postings :: Parser (Text, Vector DocumentId)
postings = do
  tokenSize <- fromIntegral <$> anyUInt64LE
  tokenBytes <- take tokenSize
  let token = E.decodeUtf8 tokenBytes
  postingsLength <- fromIntegral <$> anyUInt64LE
  postingsList <- count postingsLength anyUInt64LE
  let postings = V.fromListN postingsLength . map fromIntegral $ postingsList
  return (token, postings)

index :: Parser Index
index = do
  indexSize <- fromIntegral <$> anyUInt64LE
  tokenPostings <- count indexSize postings
  return $ HashMap.fromList tokenPostings

inv :: Parser Inv
inv = Inv <$> docs <*> index

invFrom name = (eitherResult . parse inv) <$> L.readFile name

parseData = eitherResult . parse inv

data Query = Multiple Ops [Text]
           | One Text
           deriving (Show)

data Ops = Or' | And'
         deriving (Show, Eq)

term :: PT.Parser Text
term = T.pack <$> many P.letter

or' :: PT.Parser Ops
or' = (P.string "or" <|> P.string "OR") >> return Or'

and' :: PT.Parser Ops
and' = (P.string "and" <|> P.string "AND") >> return And'

queryEnd :: PT.Parser (Ops, Text)
queryEnd = do
  operand <- or' <|> and'
  P.spaces
  t <- term
  return (operand, t)

query :: PT.Parser Query
query = do
  P.spaces
  t <- term
  P.spaces
  ts <- P.many queryEnd
  P.eof
  case ts of
   [] -> return $ One t
   ((op, _):ts') -> case all ((== op) . fst) ts of
     False -> P.parserFail "different operators"
     True  -> return $ Multiple op (t : map snd ts)

parseQuery :: Text -> Either P.ParseError Query
parseQuery = P.parse query ""

showResults result = case V.null result of
  True  -> "no documents found"
  False -> "found " ++ intercalate ", " (V.toList firstPageResults) ++ renderOthers otherResults
  where
    (firstPageResults, otherResults) = V.splitAt 2 result
    renderOthers otherResults = if V.null otherResults then "" else " and " ++ show (V.length otherResults) ++ " others"

-- type Docs = HashMap DocumentId Document
-- type Index = HashMap Text (Vector DocumentId)
-- data Inv = Inv Docs Index

run docs index query = undefined

repl docs index = loop
  where loop = do
          query <- getLine
          case null query of
            True -> return ()
            False -> do
              case parseQuery (T.pack query) of
                Left err -> putStrLn $ "incorrect query"
                Right query -> putStrLn $ showResults (run docs index query)
              loop

startRepl indexData = do
  case parseData indexData of
    Left err -> putStrLn err
    Right (Inv docs index) -> repl docs index


main = do
  [indexFile] <- getArgs
  let hashFile = addExtension indexFile "md5"
  expectedHash <- BS.readFile hashFile
  data' <- L.readFile indexFile
  let resultHash = hashlazy data'
  if resultHash /= expectedHash then do
    putStrLn $ "md5(" ++ indexFile ++ ") doesn't match with " ++ hashFile
    else startRepl data'
