{-# LANGUAGE ScopedTypeVariables #-}
{-# Language OverloadedStrings #-}

module Utils where

import StreamWeb.Types
import Control.Monad.State
import Data.Aeson
import Network.Socket.ByteString
import Data.Functor
import Data.ByteString (ByteString, intercalate)
import qualified Network.Socket as NS
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BC

addRoute :: forall e m a. Method -> RoutePattern -> Action m -> StreamW e m ()
addRoute method pat action = StreamW $ modify $ \s -> s { routes = Route method pat action : routes s}

toStrict :: BL.ByteString -> ByteString
toStrict = BS.concat . BL.toChunks

sendJson :: forall a. ToJSON a => NS.Socket -> Request -> a -> IO ()
sendJson so req res = do
  let res' = buildResponse req . toStrict . encode $ res
  print res'
  send so res' $> ()

buildResponse :: Request -> ByteString -> ByteString
buildResponse req res =
  let first = "HTTP/" <> version req <> " 200 OK"
      contentLength = BS.length res
      headers =intercalate "\r\n" [ "Content-Type: application/json"
                                  , "Connection: Closed"
                                  , "Content-Length: " <> (BC.pack . show $ contentLength)
                                  ]
   in first <> "\r\n" <> headers <> "\r\n\n" <> res
