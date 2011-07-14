{-# LANGUAGE OverloadedStrings #-}
module Main where
import BCrypt
import OpenSSL.Random
import Data.ByteString ( unpack )
import qualified Data.ByteString.Char8 as B

main :: IO ()
main = do
    seed <- randBytes 16
    salt <- genSalt 10 seed
    hash <- bcrypt "foobar" salt
    B.putStrLn salt
    B.putStrLn hash
    return ()