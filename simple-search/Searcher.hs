{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, TupleSections, BangPatterns #-}
module Main where

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

import Data.Maybe
import Data.Char
import Data.Int
import Data.Word
import Data.List (intercalate, foldl', foldl1')
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
type DocumentId = Int
type Token = Text

type TokenIndex = Int
type Posting = (DocumentId, TokenIndex)

type Docs = HashMap DocumentId Document
type Index = HashMap Text (Vector Posting)
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
  lower <- anyUInt32LE
  higher <- anyUInt32LE
  return $ makeUInt64 lower higher

anyUInt32LE = makeUInt32 <$> anyWord8 <*> anyWord8 <*> anyWord8 <*> anyWord8

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

posting  :: Parser Posting
posting = (curry (fromIntegral *** fromIntegral)) <$> anyUInt32LE <*> anyUInt32LE

postings :: Parser (Text, Vector Posting)
postings = do
  tokenSize <- fromIntegral <$> anyUInt64LE
  tokenBytes <- take tokenSize
  let token = E.decodeUtf8 tokenBytes
  postingsLength <- fromIntegral <$> anyUInt64LE
  postings <- V.fromListN postingsLength <$> count postingsLength posting
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

data PositionalDependency = Around Int
                          | Side Int
                          deriving (Show, Eq)

data Query' = Query' Text [(PositionalDependency, Text)]
            deriving (Show, Eq)

number :: Int -> PT.Parser Char -> PT.Parser Int
number base baseDigit = do
  digits <- many1 baseDigit
  let n = foldl' (\x d -> base*x + digitToInt d) 0 digits
  return n

lexeme p = P.spaces >> p

decimal = number 10 P.digit

previousD = P.char '-' >> (\n -> Side (-n)) <$> decimal
nextD = P.char '+' >> Side <$> decimal
aroundD = Around <$> decimal

dependency = (P.char '/') >> (previousD <|> nextD <|> aroundD)

queryEnd' = (,) <$> lexeme dependency <*> lexeme term

query' = do
  P.spaces
  t <- term
  ts <- P.many queryEnd'
  P.eof
  return $ Query' t ts

parseQuery' = P.parse query' ""

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

showResults :: Vector Text -> String
showResults result = case V.null result of
  True  -> "no documents found"
  False -> "found " ++ intercalate ", " (map T.unpack . V.toList $ firstPageResults) ++ renderOthers otherResults
  where
    (firstPageResults, otherResults) = V.splitAt 2 result
    renderOthers otherResults = if V.null otherResults then "" else " and " ++ show (V.length otherResults) ++ " others"


properAround !dist = \(_, !idx) (_, !idx') -> distance idx idx' <= dist
properNext !dist = \(_, !idx) (_, !idx') -> idx + dist == idx'
properPrevious !dist = \(_, !idx) (_, !idx') -> idx' + dist == idx

distance !a !b = max a b - min a b

getProperFunction (Around n) = properAround n
getProperFunction (Side n)
  | n > 0 = properNext n
  | n < 0 = properPrevious (-n)

coordinateMerge' dep left right = V.fromList $ loop (V.toList left) (V.toList right)
  where loop [] _  = []
        loop _  [] = []
        loop left right = let doc = fst . head $ left
                              splitByDoc = span ((== doc) . fst)
                              (lefts, lefts') = splitByDoc left
                              (rights, rights') = splitByDoc right
                          in go lefts rights lefts' rights'

        proper = getProperFunction dep
        go left right xs ys
          | null left || null right = loop xs ys
          | otherwise = filterC (\r -> any (\l -> proper l r) left) right (loop xs ys)
          where
            filterC p [] cont = cont
            filterC p (x:xs) cont = case p x of
              True -> x : other
              False -> other
              where other = filterC p xs cont

coordinateMerge = foldl' go
  where go left (dep, right) = coordinateMerge' dep left right

orMerge' left right = V.fromList $ loop (V.toList left) (V.toList right)
  where loop [] right' = right'
        loop left' [] = left'
        loop xs@(x:xs') ys@(y:ys') = case compare x y of
          EQ -> x : loop xs' ys'
          LT -> x : loop xs' ys
          GT -> y : loop xs  ys'

-- andMerge' left right = V.fromList $ loop (V.toList left) (V.toList right)
--   where loop [] right' = []
--         loop left' [] = []
--         loop xs@(x:xs') ys@(y:ys') = case compare x y of
--           EQ -> x : loop xs' ys'
--           LT -> loop xs' ys
--           GT -> loop xs  ys'

orMerge postings = foldr1 orMerge' postings
-- andMerge postings = foldr1 andMerge' postings

-- merging Or' = orMerge
-- merging And' = andMerge

-- run :: Hunspell -> Docs -> Index -> Query -> Vector Document
-- run hunspell docs index query = case query of
--   One term -> getDocs . orMerge . map lookupIndex . stem $ term
--   Multiple op terms -> getDocs . merging op . map (orMerge . map lookupIndex . stem) $ terms
--   where
--     lookupIndex = fromMaybe V.empty . flip HashMap.lookup index
--     getDocs = V.map (docs HashMap.!)
--     stem = stemText hunspell

mapFst f = map (first f)
mapSnd f = map (second f)

run :: Hunspell -> Docs -> Index -> Query' -> Vector Document
run hunspell docs index (Query' t ts)  = getDocs . uncurry coordinateMerge . addDeps . lookup $ terms
  where
    lookupIndex = fromMaybe V.empty . flip HashMap.lookup index
    getDocs = V.map ((docs HashMap.!) . fst)
    stem = stemText hunspell
    makeTerms t ts = t : map snd ts
    terms = makeTerms t ts
    deps = map fst ts
    addDeps (x:xs) = (x, zip deps xs)
    lookup = \terms -> map (orMerge . map lookupIndex . stem) terms

repl hunspell docs index = do
  hSetBuffering stdout NoBuffering
  loop
  where loop = do
          putStr "> "
          eof <- isEOF
          if eof then return ()
            else do
            query <- getLine
            case null query of
              True -> return ()
              False -> do
                case parseQuery' (T.pack query) of
                  Left err -> putStrLn $ "incorrect query"
                  Right query -> putStrLn $ showResults (search query)
                loop
        search = run hunspell docs index

startRepl hunspell indexData = do
  case parseData indexData of
    Left err -> putStrLn err
    Right (Inv docs index) -> repl hunspell docs index

createDefaultHunspell = makeHunspell "./hunspell-dictionaries/ru_RU.aff" "./hunspell-dictionaries/ru_RU.dic"

main = do
  [indexFile] <- getArgs
  let hashFile = addExtension indexFile "md5"
  (Just hunspell) <- createDefaultHunspell
  expectedHash <- BS.readFile hashFile
  data' <- L.readFile indexFile
  let resultHash = hashlazy data'
  if resultHash /= expectedHash then do
    putStrLn $ "md5(" ++ indexFile ++ ") doesn't match with " ++ hashFile
    else startRepl hunspell data'
