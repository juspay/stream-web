{-# Language OverloadedStrings #-}
{-# Language NamedFieldPuns #-}
module StreamWeb.AttoParser where

import Data.List         (find)
import Data.Maybe        (Maybe(..), catMaybes)
import Data.Char         (toUpper)
import Data.Functor
import StreamWeb.Types

import qualified Data.HashMap.Lazy as Map
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Internal as BI
import qualified Data.Attoparsec.ByteString as AB
import qualified Data.Attoparsec.ByteString.Char8 as AP

tillHeaderParser :: AP.Parser Request
tillHeaderParser = do
  method   <- toMethod <$> AP.takeWhile (/= ' ')
  AP.char ' '
  path    <- AP.takeTill (\c -> c == ' ' || c == '?')
  AP.choice [AP.char '?', return ' ']
  queryParams <- AP.manyTill queryP (AP.char ' ')
  protocol <- toProtocol <$> AP.takeWhile (/= '/')
  AP.char '/'
  version  <- AP.takeWhile (/='\r')
  AP.endOfLine
  headers <- AP.manyTill getHeaders checkEndOfHeader
  return (Request method path (Map.fromList queryParams) protocol version headers "")

requestParser :: Request -> AP.Parser Request
requestParser req = do
  body   <- case find (\(x,y) -> x ==  "Content-Type") (headers req) of
               Just (_, "application/json")                  -> AP.takeByteString
               Just (_, "application/x-www-form-urlencoded") -> do
                                                                 res <- urlEncodedParserToJson
                                                                 return $ "{ " <> BS.concat res  <> " }"
               Just (_,_)                                     -> AP.takeByteString
               Nothing                                        -> AP.takeByteString
  return req {body}

toMethod :: BS.ByteString -> Method
toMethod method = case BS.unpack  method of
  "GET"  -> GET
  "POST" -> POST
  "OPTIONS" -> OPTIONS
  _      -> GET

toProtocol :: BS.ByteString -> Protocol
toProtocol protocol = case BS.unpack protocol of
  "HTTP"  -> HTTP
  "HTTPS" -> HTTPS
  "FTP"   -> FTP
  _ -> HTTP

urlEncodedParserToJson = fmap catMaybes . AP.many' . AP.choice $ [Just <$> getBody , Nothing <$ checkEndOfInput]
  where
    getBody = do
              key   <- AP.takeWhile (/= '=')
              AP.char '='
              value <- AP.takeWhile (/= '&')
              AP.char '&'
              return  ("\"" <> key <> "\" : \"" <> value <> "\" , ")
    checkEndOfInput = AP.endOfInput

getHeaders :: AP.Parser (BS.ByteString, BS.ByteString)
getHeaders = do
  key <- AP.takeTill (== ':')
  AP.char ':'
  _ <- AP.takeWhile (==' ')
  value <-  AP.takeWhile (/= '\r')
  AP.endOfLine
  return (key,value)

queryP = do
  key <- AP.takeWhile (/= '=')
  AP.char '='
  value <- AP.takeTill (\x -> x == '&' || x == ' ')
  AP.choice [AP.char '&', return ' ']
  return (key, value)

checkEndOfHeader :: AP.Parser ()
checkEndOfHeader = AP.endOfLine
