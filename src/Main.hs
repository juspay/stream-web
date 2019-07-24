{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString as BS
import qualified Streamly.Prelude as SP
import qualified Streamly.Network.Socket as NS
import qualified Data.Attoparsec.ByteString.Char8 as AP
import Streamly
import Types
import Streamly.Network.Server
import Network.Socket.ByteString
import AttoParser
import Data.Char (chr)
unpackToString :: BS.ByteString -> String
unpackToString = map (chr . fromEnum) . BS.unpack

main :: IO ()
main =  do
  x <- SP.drain $ sockStream
  print  $ x

sockStream :: SerialT IO Int
sockStream = SP.concatMapBy wAsync (`NS.withSocketS` (\so -> SP.yieldM (do
                                                                           x <- recv so 4096
                                                                           print $ AP.parseOnly requestParser x
                                                                           send so "done"))) (serially $ connectionsOnAllAddrs 8081)
