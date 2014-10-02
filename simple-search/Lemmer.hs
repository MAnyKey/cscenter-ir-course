{-# LANGUAGE CPP, ForeignFunctionInterface, EmptyDataDecls #-}
module Lemmer where

import Foreign
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Foreign.ForeignPtr
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

newtype Analyzer = Analyzer (Ptr Analyzer)
newtype WordInfos = WordInfos (Ptr WordInfos)

foreign import ccall "analyzer_new" c_analyzer_new :: CString -> IO (Ptr Analyzer) -- Analyzer * analyzer_new(const char * dirname);
foreign import ccall "&analyzer_free" c_analyzer_free :: FunPtr (Ptr Analyzer -> IO ()) --void analyzer_free(Analyzer * analyzer);
--foreign import ccall "analyzer_get_word_info" c_getWordInfo :: Analyzer -- bool analyzer_get_word_info(Analyzer * analyzer, char * word, unsigned int word_size, WordInfos * buffer);

makeLemmer :: FilePath -> IO (Maybe Lemmer)
makeLemmer path = B.useAsCString (C.pack path) $ \c_path -> do
  analyzer_ptr <- c_analyzer_new c_path
  if analyzer_ptr == nullPtr then return Nothing
    else do
    analyzer <- newForeignPtr c_analyzer_free analyzer_ptr
    return $ Just (Lemmer analyzer)
    
  
    

data Lemmer = Lemmer !(ForeignPtr Analyzer)
            deriving (Eq, Show)
