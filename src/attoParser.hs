{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Attoparsec.Text as AP
import qualified Data.Text.Internal as T
import Data.List (find)



data Method = GET | POST deriving (Show)
data Protocol = HTTP | HTTPS | FTP deriving (Show)
data Request = Request
      {  method :: T.Text
        ,path   :: T.Text
        ,protocol :: T.Text
        ,version :: T.Text
        ,headers :: [(T.Text , T.Text)]
        ,body    :: T.Text
      } deriving (Show)


notSpace :: Char -> Bool
notSpace ' ' = False
notSpace  _  = True

notSlash :: Char -> Bool
notSlash '/' = False
notSlash  _  = True

notEndOfLine :: Char -> Bool
notEndOfLine '\r' = False
notEndOfLine '\n' = False
notEndOfLine  _   = True

notColon :: Char -> Bool
notColon ':' = False
notColon  _  = True

endOfInput :: Char -> Bool
endOfInput _ = True

notAmpersand :: Char -> Bool
notAmpersand '&' = False
notAmpersand  _  = True

notEqualSign:: Char -> Bool
notEqualSign '=' = False
notEqualSign  _  = True

flattenTextList :: [T.Text] -> T.Text
flattenTextList (x:xs) =  x <> (flattenTextList xs)
flattenTextList  _     =  ""
--GET / HTTP/1.1\r\nHost: localhost:8081\r\nUser-Agent: curl/7.64.0\r\nAccept: */*\r\n\r\n"
--requestParser :: AP.Parser ByteString
applicationJsonParser = AP.takeWhile endOfInput


urlEncodedParserToJson = AP.many' $ do
  key   <- AP.takeWhile notEqualSign
  AP.char '='
  value <- AP.takeWhile notAmpersand
  AP.choice [AP.char '&',endOfBody]
  return  ("\"" <> key <> "\" : \"" <> value <> "\" , ")
  where
    endOfBody = do
      AP.endOfInput
      return '|'

requestParser = do
  method <- AP.takeWhile notSpace
  AP.char ' '
  path   <- AP.takeWhile notSpace
  AP.char ' '
  protocol <- AP.takeWhile notSlash
  AP.char '/'
  version <- AP.takeWhile notEndOfLine
  AP.endOfLine
  headers <- AP.many' $  AP.choice [checkEndOfHeader , getHeaders]  -- stops when it reached \r\n\r\n
  body   <- case find (\(x,y) -> x == "Content-Type") headers of
               Just (_, " application/json")                  -> applicationJsonParser --returns the body as it is
               Just (_, " application/x-www-form-urlencoded") -> do
                                                                 res <- urlEncodedParserToJson
                                                                 return $ "{ " <> (flattenTextList res ) <> " }"  --flattenning the list maybe a bottleneck
               Just (_,_)                                     -> applicationJsonParser
               Nothing                                        -> applicationJsonParser
  return Request {
                  method    = method
                 ,path      = path
                 ,protocol  = protocol
                 ,version   = version
                 ,headers   = headers
                 ,body      = body
                 }
  where
    checkEndOfHeader = AP.endOfLine >> return ("a","b")
    getHeaders = do
                 key <- AP.takeWhile notColon
                 AP.char ':'
                 value <- AP.takeWhile notEndOfLine
                 AP.endOfLine
                 return  (key,value)


main :: IO ()
main = print $ AP.parseOnly requestParser "GET / HTTP/1.1\r\nHost: localhost:8081\r\nUser-Agent: curl/7.64.0\r\nAccept: */*\r\n\r\n"

{-
REQUESTS I CHEKED THIS PARSER ON ->
1. "GET / HTTP/1.1\r\nHost: localhost:8081\r\nUser-Agent: curl/7.64.0\r\nAccept: */*\r\n\r\n"
2. "POST / HTTP/1.1\r\nHost: localhost:8081\r\nUser-Agent: curl/7.64.0\r\nAccept: */*\r\n\r\n"
3. "POST /ecr/orders HTTP/1.1\r\nHost: localhost:8081\r\nUser-Agent: curl/7.64.0\r\nAccept: */*\r\nAuthorization: Basic N0JBRDA4OUExRjYwNEYwREEyNDlCNjY3QTVCMUQzM0Q6\r\nContent-Type: application/
x-www-form-urlencoded\r\nPostman-Token: 76594145-0873-4504-9f60-f67184a87218\r\ncache-control: no-cache\r\nversion: 2018-07-01\r\nContent-Length: 102\r\n\r\norder_id=1552475197433&amount=3.01
&currency=INR&description=hello&return_url=http%3A%2F%2Fjuspay.in%2F"
4. "POST /ecr/orders HTTP/1.1\r\nHost: localhost:8081\r\nUser-Agent: curl/7.64.0\r\nAccept: */*\r\nAuthorization: Basic N0JBRDA4OUExRjYwNEYwREEyNDlCNjY3QTVCMUQzM0Q6\r\nContent-Type: application/
x-www-form-urlencoded\r\nPostman-Token: 76594145-0873-4504-9f60-f67184a87218\r\ncache-control: no-cache\r\nversion: 2018-07-01\r\nContent-Length: 102\r\n\r\norder_id=1552475197433&amount=3.01
&currency=INR&description=hello&return_url=http%3A%2F%2Fjuspay.in%2F"

5. "GET /done/id=123/name=this HTTP/1.1\r\nHost: localhost:8081\r\nUser-Agent: curl/7.64.0\r\nAccept: */*\r\n\r\n"
6. "POST /checkConfig HTTP/1.1\r\nHost: localhost:8081\r\nUser-Agent: curl/7.64.0\r\nAccept: */*\r\nContent-Type: application/json\r\nPostman-Token: f89b2256-392e-4b09-a307-362b79849852\r\ncache
-control: no-cache\r\nContent-Length: 384\r\n\r\n\n\n{\n  \"scalingActions\" : [\n    { \"deployment\" : \"euler-autoscaler-65bfd7279\"\n    , \"namespace\" : \"lambda\"\n    , \"rules\" : [\
n      { \"metric\" : \"custom_cpu\"\n      , \"desired\" : 50.00\n      },\n      { \"metric\" : \"istio_rps\"\n      , \"desired\" : 50.00\n      }\n    ]\n    , \"schedules\" : [\n      {
\"time\": \"2019-06-11T18:29:00Z\"\n      , \"min\" : 6\n      , \"max\" : 4\n      }\n    ]\n    }\n  ]\n}"
-}
