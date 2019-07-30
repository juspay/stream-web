module Main where

import StreamWeb
import Network.Socket.ByteString (recv, send)
import Utils

main :: IO ()
main =  startServer (\s r -> sendJson s r "hie")
