module AoC2015.Day04 where

import Control.Monad (guard)
import Crypto.Hash.MD5 (finalize, hash)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as BS16
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Linear.V2 (V2 (V2))

parse :: String -> BS.ByteString
parse = encodeUtf8 . T.pack . init

solve :: BS.ByteString -> BS.ByteString -> Int
solve zeros prefix = head $ do
  num <- [1 ..]
  let suffix = encodeUtf8 . T.pack . show $ num
  let res = BS16.encode $ hash $ prefix <> suffix
  guard (BS.isPrefixOf zeros res)
  pure num

run :: String -> IO ()
run xs = do
  let parsed = parse xs
  let a = solve "00000" parsed
  let b = solve "000000" parsed
  print a
  print b
