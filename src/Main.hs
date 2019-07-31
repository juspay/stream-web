{-# Language OverloadedStrings #-}
module Main where

import StreamWeb
import Network.Socket.ByteString (recv, send)
import StreamWeb.Utils
import Control.Exception
import qualified Streamly.Prelude as SP

main :: IO ()
main = SP.drain $ startServer 8081
