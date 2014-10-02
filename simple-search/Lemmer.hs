{-# LANGUAGE CPP, ForeignFunctionInterface, EmptyDataDecls, DeriveDataTypeable #-}
module Lemmer where

import Foreign
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Foreign.ForeignPtr
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Control.Exception
import Data.Typeable

newtype Analyzer = Analyzer (Ptr Analyzer)
newtype WordInfos = WordInfos (Ptr WordInfos)

foreign import ccall "analyzer_new" c_analyzer_new :: CString -> IO (Ptr Analyzer)
-- Analyzer * analyzer_new(const char * dirname);
foreign import ccall "&analyzer_free" c_analyzer_free :: FunPtr (Ptr Analyzer -> IO ())
-- void analyzer_free(Analyzer * analyzer);
foreign import ccall "analyzer_get_word_info" c_analyzer_get_word_info :: Ptr Analyzer -> CString -> CUInt -> Ptr WordInfos -> IO ()
-- bool analyzer_get_word_info(Analyzer * analyzer, char * word, unsigned int word_size, WordInfos * buffer);

foreign import ccall "infos_new" c_infos_new :: CUInt -> IO (Ptr WordInfos)
-- struct WordInfos * infos_new(unsigned int max_size);
foreign import ccall "&infos_free" c_infos_free :: FunPtr (Ptr WordInfos -> IO ())
-- void infos_free(struct WordInfos * wi);

foreign import ccall "infos_get_size" c_infos_get_size :: Ptr WordInfos -> CUInt
-- int infos_get_size(struct WordInfos * wi);
foreign import ccall "infos_get_normal_form" c_infos_get_normal_form :: Ptr WordInfos -> CUInt -> CString
-- char * infos_get_normal_form(struct WordInfos * wi, unsigned int id);

makeLemmer :: FilePath -> IO (Maybe Lemmer)
makeLemmer path = B.useAsCString (C.pack path) $ \c_path -> do
  analyzer_ptr <- c_analyzer_new c_path
  if analyzer_ptr == nullPtr then return Nothing
    else do
    analyzer <- newForeignPtr c_analyzer_free analyzer_ptr
    return $ Just (Lemmer analyzer)

data CannotAllocateWordInfos = CannotAllocateWordInfos
                               deriving (Show, Typeable)
instance Exception CannotAllocateWordInfos

withInfos :: Int -> (Infos -> IO b) -> IO b
withInfos size f = do
  infosPtr <- c_infos_new (fromIntegral size)
  if infosPtr == nullPtr then throwIO CannotAllocateWordInfos
    else do
    infos <- newForeignPtr c_infos_free infosPtr
    f (Infos infos)

data Infos = Infos !(ForeignPtr WordInfos)
           deriving (Eq, Show)

data Lemmer = Lemmer !(ForeignPtr Analyzer)
            deriving (Eq, Show)
