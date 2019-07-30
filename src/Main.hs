{-# Language OverloadedStrings #-}
module Main where

import StreamWeb
import Network.Socket.ByteString (recv, send)

main :: IO ()
main =  startServer (\s r -> send s "HTTP/1.1 200 OK\r\nContent-Type: text\r\nConnection: Closed\r\nContent-Length: 4\r\n\nthis" )
