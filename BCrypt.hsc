{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

-- Copyright 2011 Brett Carter brett@rdnzl.net
-- All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:
-- 1. Redistributions of source code must retain the above copyright
--    notice, this list of conditions and the following disclaimer.
-- 2. Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
-- 3. All advertising materials mentioning features or use of this software
--    must display the following acknowledgement:
--      This product includes software developed by Niels Provos.
-- 4. The name of the author may not be used to endorse or promote products
--    derived from this software without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
-- IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
-- OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
-- IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
-- INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
-- NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
-- DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
-- THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
-- (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
-- THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

-- This is a thin haskell wrapper implementing bcrypt using the blowfish
-- implementation from OpenBSD's libc

module BCrypt
    ( bcrypt
    , genSalt
    )
where

#include <stdlib.h>
#include "blf.h"
#include "bcrypt.h"

import Foreign
import Foreign.C.Types
import Foreign.C.String
import qualified Data.ByteString.Unsafe as B
import qualified Data.ByteString.Internal as B ( fromForeignPtr
                                               , c_strlen
                                               )
import qualified Data.ByteString as B

-- Seed should be a 16 character bytestring from a secure rando
genSalt :: Integer -> B.ByteString -> IO B.ByteString
genSalt cost seed = do
        out <- mallocBytes 30 -- BCRYPT_SALT_OUTPUT_SIZE
        B.useAsCString seed $ \s -> do
             salty <- c_bcrypt_gensalt out (fromIntegral cost::CInt) (castPtr s)
             B.packCString salty

bcrypt :: B.ByteString -> B.ByteString -> IO B.ByteString
bcrypt key salt = do
       out <- mallocBytes 128 -- BCRYPT_MAX_OUTPUT_SIZE
       B.useAsCString key $ \k -> B.useAsCString salt $ \s -> do
           B.packCString $ c_bcrypt out k s

foreign import ccall unsafe "bcrypt.h bcrypt_gensalt"
    c_bcrypt_gensalt :: CString -> CInt -> Ptr CUInt -> IO CString

foreign import ccall unsafe "bcrypt.h bcrypt"
    c_bcrypt :: CString -> CString -> CString -> CString
