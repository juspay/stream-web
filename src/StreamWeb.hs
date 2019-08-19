{-# Language OverloadedStrings #-}
module StreamWeb (startServer, sendJson, sendStatus, sendWithStatus, corsHeaders) where

import Streamly                  (SerialT, wAsync, serially, IsStream)
import Streamly.Network.Server   (connectionsOnAllAddrs)
import Network.Socket.ByteString (recv, send)
import Data.List                 (find)
import Data.Functor              (($>))
import StreamWeb.Utils           (lowercase, sendStatus, sendJson, sendWithStatus, corsHeaders)
import StreamWeb.AttoParser      (tillHeaderParser, requestParser)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Streamly.Prelude as SP
import qualified Streamly.Network.Socket as NS
import qualified Data.Attoparsec.ByteString.Char8 as AP
import qualified StreamWeb.Types as T
import qualified Network.Socket as NS

startServer :: IsStream t => NS.PortNumber -> t IO (NS.Socket, T.Request)
startServer portNumber =
  SP.mapMaybe id $
    SP.concatMapBy wAsync
       (\so ->
          SP.yieldM $ do
            x <- recv so 2048 -- maximum size of headers
            case AP.parse tillHeaderParser x of
              AP.Done rem r ->
                if T.method r == T.OPTIONS then sendWithStatus so 200 "OK" corsHeaders $> Nothing
                else
                  case find (\(x,y) -> lowercase x == "content-length") (T.headers r) of
                      Just (_, len) -> do
                        body <- if BS.length rem < read (BC.unpack len)
                                then (rem <>) <$> recv so (read (BC.unpack len) - BS.length rem)
                                else return rem
                        case AP.parseOnly (requestParser r) body of
                          Right req -> return $ Just (so, req)
                          Left  err -> send so (BC.pack err) $> Nothing
                      Nothing -> if T.method r == T.GET
                                then return $ Just (so, r)
                                else print "No Content-Length header field" *> sendStatus so 411 $> Nothing
              _ -> print "Parse Failed because all the headers are not received" *> sendStatus so 413 $> Nothing
        )
        (serially $ connectionsOnAllAddrs portNumber)
