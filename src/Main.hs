module Main where

import StreamWeb (startServer)
import qualified Streamly.Prelude as SP

main :: IO ()
main = SP.drain $ startServer 8081
