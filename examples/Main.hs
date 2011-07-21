{-# LANGUAGE OverloadedStrings #-}
module Main where
import Data.Digest.BCrypt
import Control.Monad.Error
import OpenSSL.Random
import Data.ByteString ( unpack )
import Data.ByteString.Char8 ( pack )
import qualified Data.ByteString.Char8 as B


example :: IO ()
example = do
    seed    <- randBytes 16
    badSeed <- randBytes 10
    let salt = genSalt 10 seed
        badSalt = genSalt 10 badSeed
        hashed = maybeHash "foobar" salt
        hashedBad = maybeHash "foobar" badSalt
    print salt
    B.putStrLn hashed
    print badSalt
    B.putStrLn hashedBad

main :: IO ()
main =
     forM_ [1..100000] $ \n -> do
                seed <- randBytes 16
                print $ leakTest' n seed

leakTest' :: Int -> B.ByteString -> B.ByteString
leakTest' n seed = do
    maybeHash (pack $ show n) $ genSalt 4 seed

maybeHash :: B.ByteString -> Either String BSalt -> B.ByteString
maybeHash val salt = case salt of
                         Right salt' -> bcrypt val salt'
                         Left er -> error er
