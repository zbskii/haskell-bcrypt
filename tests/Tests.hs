{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Monad
import Test.QuickCheck
import Test.QuickCheck.Gen
import Data.Digest.BCrypt
import System.Random
import System.Exit
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8

main :: IO ()
main = do
  res <- quickCheckResult bcryptCheck
  case res of
         Success{} -> exitSuccess
         _ -> exitFailure


bcryptCheck :: B.ByteString -> BSalt -> Bool
bcryptCheck plain salt = encrypted == encrypted'
    where
        ct = bcrypt plain salt
        encrypted = case packBSalt ct of
            Just s -> s
            _ -> error "Impossible packing of salt"
        encrypted' = case packBSalt $ bcrypt plain encrypted of
            Just s' -> s'
            _ -> error "Impossible packing of salt #2"



instance Arbitrary B.ByteString where
    arbitrary = do
        count <- choose (3, 15)
        let char = choose ('a', 'z')
        B8.pack <$> replicateM count char


instance Arbitrary BSalt where
    arbitrary = salting

salting = MkGen (\r _ -> mkSalt r)

get16Bytes g = B.pack . take 16 . map fromInteger . randomRs (0, 255) $ g

mkSalt :: RandomGen g => g -> BSalt
mkSalt g = case genSalt 4 bytes of
       Just salted -> salted
       _ -> error $ "Bad bsalt " ++ (B8.unpack bytes)
       where
           bytes = get16Bytes g
