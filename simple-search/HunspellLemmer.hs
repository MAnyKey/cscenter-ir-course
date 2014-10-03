{-# LANGUAGE CPP, ForeignFunctionInterface, EmptyDataDecls, DeriveDataTypeable #-}
module HunspellLemmer  (Hunspell, makeHunspell, analyzeText, analyze) where

import Data.Typeable
import System.IO.Unsafe

import Control.Monad
import Control.Exception

import Foreign hiding (unsafePerformIO)
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Foreign.ForeignPtr

import Data.ByteString (ByteString)
import qualified Data.ByteString as B

import Data.Vector (Vector)
import qualified Data.Vector as V

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as E

newtype Hunhandle = Hunhandle (Ptr Hunhandle)
type CStringList = (Ptr CString)

foreign import ccall "Hunspell_create" c_hunspell_create :: CString -> CString -> IO (Ptr Hunhandle)
foreign import ccall "&Hunspell_destroy" c_hunspell_destroy :: FunPtr (Ptr Hunhandle -> IO ())
foreign import ccall "Hunspell_analyze" c_hunspell_analyze :: Ptr Hunhandle -> Ptr CStringList -> CString -> IO CInt
foreign import ccall "Hunspell_free_list" c_hunspell_free_list :: Ptr Hunhandle -> Ptr CStringList -> CInt -> IO ()

type FreeList = Ptr (Ptr CString) -> IO ()
foreign import ccall "wrapper" mkFreeList :: FreeList -> IO (FunPtr FreeList)

data Hunspell = Hunspell !(ForeignPtr Hunhandle)
              deriving (Show, Eq)

makeHunspell :: FilePath -> FilePath -> IO (Maybe Hunspell)
makeHunspell aff_path dic_path =
  withCString aff_path $ \c_aff_path -> do
  withCString dic_path $ \c_dic_path -> do
    hunhandlePtr <- c_hunspell_create c_aff_path c_dic_path
    if hunhandlePtr == nullPtr then return Nothing
      else do
      hunhandle <- newForeignPtr c_hunspell_destroy hunhandlePtr
      return $ Just (Hunspell hunhandle)

withFreeList f action = bracket (mkFreeList f) freeHaskellFunPtr action

withStringList handle p_string_list size action = do
  let free list = c_hunspell_free_list handle list size
  withFreeList free $ \freeList -> do
    string_list <- newForeignPtr freeList p_string_list
    withForeignPtr string_list $ \p_string_list -> do
      peek p_string_list >>= action

ptrsToByteStrings :: CStringList -> CInt -> IO [ByteString]
ptrsToByteStrings list c_size =
  mapM (peekElemOff list >=> B.packCString) . take (fromIntegral c_size) $ [0..]

ptrsToByteStrings' :: CStringList -> CInt -> IO (Vector ByteString)
ptrsToByteStrings' list =
  V.mapM (peekElemOff list >=> B.packCString) . V.enumFromN 0 . fromIntegral

strings :: Ptr Hunhandle -> Ptr CStringList -> CInt -> IO [ByteString]
strings handle p_string_list size =
  withStringList handle p_string_list size $ \string_list -> do
    ptrsToByteStrings string_list size

{-# NOINLINE analyze #-}
analyze :: Hunspell -> ByteString -> [ByteString]
analyze (Hunspell handle) str = unsafePerformIO $ do
  withForeignPtr handle $ \handle -> do
  B.useAsCString str $ \c_str -> do
  alloca $ \p_string_list -> do
    size <- c_hunspell_analyze handle p_string_list c_str
    strings handle p_string_list size

analyzeText :: Hunspell -> Text -> [Text]
analyzeText hunspell = map E.decodeUtf8 . analyze hunspell . E.encodeUtf8
