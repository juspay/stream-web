{-# Language OverloadedStrings #-}
module StreamWeb (startServer, sendJson) where

import Streamly                  (SerialT, wAsync, serially)
import Streamly.Network.Server   (connectionsOnAllAddrs)
import Network.Socket.ByteString (recv, send)
import Data.List                 (find)
import Data.Functor              (($>))
import StreamWeb.Utils           (lowercase, sendStatus, sendJson)
import StreamWeb.AttoParser      (tillHeaderParser, requestParser)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Streamly.Prelude as SP
import qualified Streamly.Network.Socket as NS
import qualified Data.Attoparsec.ByteString.Char8 as AP
import qualified StreamWeb.Types as T
import qualified Network.Socket as NS

startServer :: NS.PortNumber -> SerialT IO (NS.Socket, T.Request)
startServer portNumber =
  SP.mapMaybe id $ SP.concatMapBy wAsync
                        (`NS.withSocketS`
                            (\so ->
                               SP.yieldM $ do
                                  x <- recv so 2048 -- maximum size of headers
                                  case AP.parse tillHeaderParser x of
                                    AP.Done rem r ->
                                      case find (\(x,y) -> lowercase x == "content-length") (T.headers r) of
                                          Just (_, len) -> do
                                             body <- if BS.length rem < read (BC.unpack len)
                                                    then (rem <>) <$> recv so (read (BC.unpack len) - BS.length rem)
                                                    else return rem
                                             case AP.parseOnly (requestParser r) body of
                                                Right req -> sendJson so req ("OK" :: String) $> Just (so, req)
                                                Left  err -> send so (BC.pack err) $> Nothing
                                          Nothing -> print "No Content-Length header field" *> sendStatus so 413 $> Nothing
                                    _ -> print "Parse Failed" *> sendStatus so 413 $> Nothing
                            )
                        ) (serially $ connectionsOnAllAddrs portNumber)
