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

testCompare :: IO ()
testCompare = do
  seed <- randBytes 16
  let hashed = maybeHash "foobar" salt
      salt = genSalt 10 seed
  print  hashed
  putStrLn "packing hashed..."
  let packed = packBSalt hashed
  print packed
  print $ maybeHash "foobar" packed
  let badpack = packBSalt "bah"
  print badpack
  putStrLn "Done."
  return ()

leaktest :: IO ()
leaktest =
     forM_ [1..100000] $ \n -> do
                seed <- randBytes 16
                print $ leakTest' n seed

main :: IO ()
main = example >> testCompare >> leaktest

leakTest' :: Int -> B.ByteString -> B.ByteString
leakTest' n seed = do
    maybeHash (pack $ show n) $ genSalt 4 seed

maybeHash :: B.ByteString -> Maybe BSalt -> B.ByteString
maybeHash val salt = case salt of
                         Just salt' -> bcrypt val salt'
                         _ -> ""
