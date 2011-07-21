{-# LANGUAGE ForeignFunctionInterface #-}

module Data.Digest.BCrypt
    ( bcrypt
    , genSalt
    , BSalt
    )
where

#include <stdlib.h>
#include "blf.h"
#include "bcrypt.h"

import Foreign
import Foreign.C.Types
import Foreign.C.String
import qualified System.IO.Unsafe ( unsafePerformIO )
import qualified Data.ByteString.Unsafe as B
import qualified Data.ByteString.Internal as B ( fromForeignPtr
                                               , c_strlen
                                               )
import qualified Data.ByteString as B

newtype BSalt = BSalt B.ByteString deriving (Show)

-- |Given a cost from 4-32 and a random seed of 16 bytes generate a salt.
-- Seed should be 16 bytes from a secure random generator
genSalt :: Monad m => Integer -> B.ByteString -> m BSalt
genSalt cost seed
       | B.length seed /= 16 = fail "Bad seed size"
       | otherwise = return $ unsafePerformIO $
        B.useAsCString seed $ \s ->
             allocaBytes #{const BCRYPT_SALT_OUTPUT_SIZE} $ \out -> do
                 let seed' = (fromIntegral cost::CInt)
                     bsalt = c_bcrypt_gensalt out seed' (castPtr s)
                 result <- B.packCString bsalt
                 return $ BSalt result

-- |Hash a password based on a BSalt with a given cost
bcrypt :: B.ByteString -> BSalt -> B.ByteString
bcrypt key (BSalt salt) = unsafePerformIO $
       B.useAsCString key $ \k -> B.useAsCString salt $ \s ->
           allocaBytes #{const BCRYPT_OUTPUT_SIZE} $ \out -> do
               B.packCString $ c_bcrypt out k s

foreign import ccall unsafe "bcrypt.h bcrypt_gensalt"
    c_bcrypt_gensalt :: CString -> CInt -> Ptr CUInt -> CString

foreign import ccall unsafe "bcrypt.h bcrypt"
    c_bcrypt :: CString -> CString -> CString -> CString
