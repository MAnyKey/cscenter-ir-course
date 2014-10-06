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
import Data.Int

import Data.IORef
--import Data.Digest.Pure.MD5
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
import qualified Data.Attoparsec.Text as AttT

import Crypto.Hash.MD5

import HunspellLemmer

type Document = Text
type DocumentId = Int64
type Token = Text

type Docs = HashMap DocumentId Document
type Index = HashMap Text (Vector DocumentId)
data Inv = Inv Docs Index

docs :: Parser Docs
docs = undefined

index :: Parser Index
index = undefined

inv :: Parser Inv
inv = Inv <$> docs <*> index

parseData = eitherResult . parse inv

query = undefined

parseQuery = AttT.eitherResult . AttT.parse query


printResults result = case V.null result of
  True  -> putStrLn "no documents found"
  False -> undefined
  where
    (firstPageResults, otherResults) = V.splitAt 2 result

run docs index query = undefined

repl docs index = loop
  where loop = do
          query <- getLine
          case null query of
            True -> return ()
            False -> do
              case parseQuery (T.pack query) of
                Left err -> putStrLn $ "incorrect query(" ++ err ++ ")"
                Right query -> printResults (run docs index query)
              loop

foo indexData = do
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
    else foo data'
