{-# LANGUAGE ScopedTypeVariables #-}
{-# Language OverloadedStrings #-}

module StreamWeb.Utils where

import StreamWeb.Types
import Control.Monad.State
import Data.Aeson
import Network.Socket.ByteString
import Data.Functor
import Data.Word
import Data.Char
import Data.Array.Unboxed
import Data.ByteString (ByteString, intercalate)
import qualified Network.Socket as NS
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BC

addRoute :: forall e m a. Method -> RoutePattern -> Action m -> StreamW e m ()
addRoute method pat action = StreamW $ modify $ \s -> s { routes = Route method pat action : routes s}

toStrict :: BL.ByteString -> ByteString
toStrict = BS.concat . BL.toChunks

ctypeLower = listArray (0,255) (map (BI.c2w . toLower) ['\0'..'\255']) :: UArray Word8 Word8

lowercase = BS.map (ctypeLower!)

sendJson :: forall a. ToJSON a => NS.Socket -> Request -> a -> IO ()
sendJson so req res = do
  let res' = buildResponse req . toStrict . encode $ res
  print res'
  send so res' *> NS.close so

sendStatus :: NS.Socket -> Int -> IO ()
sendStatus so status = do
  let first = "HTTP/1.1" <> BC.pack (show status) <> " " <> getMessage status
  send so first *> NS.close so

getMessage :: Int -> ByteString
getMessage 200 = "OK"
getMessage 411 = "Length Required"
getMessage 413 = "Request Entity Too Large"
getMessage 500 = "Internal Server Error"

buildResponse :: Request -> ByteString -> ByteString
buildResponse req res =
  let first = "HTTP/" <> version req <> " 200 OK"
      contentLength = BS.length res
      headers =intercalate "\r\n" [ "Content-Type: application/json"
                                  , "Connection: Closed"
                                  , "Content-Length: " <> (BC.pack . show $ contentLength)
                                  ]
   in first <> "\r\n" <> headers <> "\r\n\n" <> res
