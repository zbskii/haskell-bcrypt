{-# LANGUAGE OverloadedStrings #-}
module Main where
import BCrypt
import OpenSSL.Random
import Data.ByteString ( unpack )
import qualified Data.ByteString.Char8 as B


main :: IO ()
main = do
    seed    <- randBytes 16
    badSeed <- randBytes 10
    let salt = genSalt 10 seed :: Maybe B.ByteString
        badSalt = genSalt 10 badSeed
        hashed = maybeHash "foobar" salt
        hashedBad = maybeHash "foobar" badSalt
    print salt
    B.putStrLn hashed
    print badSalt
    B.putStrLn hashedBad
    return ()


maybeHash :: B.ByteString -> Maybe B.ByteString -> B.ByteString
maybeHash val salt = case salt of
                         Just salt' -> bcrypt val salt'
                         Nothing -> "Bad Seed." :: B.ByteString
