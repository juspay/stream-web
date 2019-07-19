{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString as BS
import qualified Streamly.Prelude as SP
import qualified Streamly.Network.Socket as NS
import Streamly
import Types
import Streamly.Network.Server
import Network.Socket.ByteString

main :: IO ()
main = SP.drain sockStream

sockStream :: SerialT IO Int
sockStream = SP.concatMapBy wAsync (`NS.withSocketS` (\so -> SP.yieldM (do
                                                                           print =<< recv so 4096
                                                                           send so "done"))) (serially $ connectionsOnAllAddrs 8081)
