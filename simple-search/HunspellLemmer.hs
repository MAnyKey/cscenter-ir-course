{-# LANGUAGE CPP, ForeignFunctionInterface, EmptyDataDecls, DeriveDataTypeable #-}
module HunspellLemmer  (Hunspell, makeHunspell, stemText, stem) where

import Data.Typeable
import System.IO.Unsafe

import Data.Monoid
import Control.Monad
import Control.Exception

import Foreign hiding (unsafePerformIO)
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import qualified Foreign.Concurrent as FC
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
foreign import ccall "Hunspell_stem" c_hunspell_stem :: Ptr Hunhandle -> Ptr CStringList -> CString -> IO CInt
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

withStringList handle p_string_list size = bracket (return p_string_list) free
  where free list = c_hunspell_free_list handle list size

handleEmptyList p_list c_size action = case c_size == 0 of
  True  -> return mempty
  False -> peek p_list >>= action

strings :: Ptr CStringList -> CInt -> IO [ByteString]
strings p_list c_size = handleEmptyList p_list c_size $ \list -> do
    mapM (peekElemOff list >=> B.packCString) . take (fromIntegral c_size) $ [0..]

strings' :: Ptr CStringList -> CInt -> IO (Vector ByteString)
strings' p_list c_size = handleEmptyList p_list c_size $ \list -> do
  V.mapM (peekElemOff list >=> B.packCString) . V.enumFromN 0 . fromIntegral $ c_size


{-# NOINLINE stem #-}
stem :: Hunspell -> ByteString -> [ByteString]
stem (Hunspell handle) str = unsafePerformIO $ do
  withForeignPtr handle $ \handle -> do
  B.useAsCString str $ \c_str -> do
  alloca $ \p_string_list -> do
    size <- c_hunspell_stem handle p_string_list c_str
    withStringList handle p_string_list size $ \string_list -> do
      strings string_list size

stemText :: Hunspell -> Text -> [Text]
stemText hunspell = map E.decodeUtf8 . stem hunspell . E.encodeUtf8
