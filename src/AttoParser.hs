{-# Language OverloadedStrings #-}
module AttoParser where

import qualified Data.Attoparsec.ByteString.Char8 as AP
import qualified Data.ByteString.Char8 as BS
import Data.List (find)
import Debug.Trace
import Data.Maybe
import Data.Char(toUpper)

data Method = GET | POST deriving (Show)
data Protocol = HTTP | HTTPS | FTP deriving (Show)
data Request = Request
      {  method   :: Method
        ,path     :: BS.ByteString
        ,protocol :: Protocol
        ,version  :: BS.ByteString
        ,headers  :: [(BS.ByteString , BS.ByteString)]
        ,body     :: BS.ByteString
      } deriving (Show)

toMethod :: BS.ByteString -> Method
toMethod method = case toUpper <$> (BS.unpack  method) of
  "GET"  -> GET
  "POST" -> POST
  _      -> GET

toProtocol :: BS.ByteString -> Protocol
toProtocol protocol = case toUpper <$> (BS.unpack protocol) of
  "HTTP"  -> HTTP
  "HTTPS" -> HTTPS
  "FTP"   -> FTP
  _ -> HTTP

urlEncodedParserToJson = fmap catMaybes $ AP.many' $
                AP.choice [Just <$> getBody , Nothing <$ checkEndOfInput]
  where
    getBody = do
              key   <- AP.takeWhile (/= '=')
              AP.char '='
              value <- AP.takeWhile (/= '&')
              AP.char '&'
              return  ("\"" <> key <> "\" : \"" <> value <> "\" , ")
    checkEndOfInput = AP.endOfInput

requestParser = do
  method   <- toMethod <$> AP.takeWhile (/= ' ')
  AP.char ' '
  path     <- AP.takeWhile (/= ' ')
  AP.char ' '
  protocol <- toProtocol <$> AP.takeWhile (/= '/')
  AP.char '/'
  version  <- AP.takeWhile (/='\r')
  AP.endOfLine
  headers <- AP.manyTill getHeaders checkEndOfHeader
  body   <- case find (\(x,y) -> x ==  "Content-Type") headers of
               Just (_, " application/json")                  -> AP.takeByteString
               Just (_, " application/x-www-form-urlencoded") -> do
                                                                 res <- urlEncodedParserToJson
                                                                 return $ "{ " <> (BS.concat res ) <> " }"
               Just (_,_)                                     -> AP.takeByteString
               Nothing                                        -> AP.takeByteString
  return Request {
                  method    = method
                 ,path      = path
                 ,protocol  = protocol
                 ,version   = version
                 ,headers   = headers
                 ,body      = body
                 }
  where
    getHeaders = do
      key <- AP.takeTill (== ':')
      AP.char(':')
      _ <- AP.takeWhile (==' ')
      value <-  AP.takeWhile (/= '\r')
      AP.endOfLine
      return (key,value)
    checkEndOfHeader = AP.endOfLine
