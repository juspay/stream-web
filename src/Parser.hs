module Parser where

import Text.Parsec
import Prelude
import Data.List
import Data.Char(toUpper)
--URL to JSON done
--Data type for method and protocol Done
--handle query params
{-

"GET / HTTP/1.1\r\nHost: localhost:8081\r\nUser-Agent: curl/7.64.0\r\nAccept: */*\r\n\r\n"
"POST / HTTP/1.1\r\nHost: localhost:8081\r\nUser-Agent: curl/7.64.0\r\nAccept: */*\r\n\r\n"
"POST /ecr/orders HTTP/1.1\r\nHost: localhost:8081\r\nUser-Agent: curl/7.64.0\r\nAccept: */*\r\nAuthorization: Basic N0JBRDA4OUExRjYwNEYwREEyNDlCNjY3QTVCMUQzM0Q6\r\nContent-Type: application/
x-www-form-urlencoded\r\nPostman-Token: 76594145-0873-4504-9f60-f67184a87218\r\ncache-control: no-cache\r\nversion: 2018-07-01\r\nContent-Length: 102\r\n\r\norder_id=1552475197433&amount=3.01
&currency=INR&description=hello&return_url=http%3A%2F%2Fjuspay.in%2F"
"POST /ecr/orders HTTP/1.1\r\nHost: localhost:8081\r\nUser-Agent: curl/7.64.0\r\nAccept: */*\r\nAuthorization: Basic N0JBRDA4OUExRjYwNEYwREEyNDlCNjY3QTVCMUQzM0Q6\r\nContent-Type: application/
x-www-form-urlencoded\r\nPostman-Token: 76594145-0873-4504-9f60-f67184a87218\r\ncache-control: no-cache\r\nversion: 2018-07-01\r\nContent-Length: 102\r\n\r\norder_id=1552475197433&amount=3.01
&currency=INR&description=hello&return_url=http%3A%2F%2Fjuspay.in%2F"

"GET /done/id=123/name=this HTTP/1.1\r\nHost: localhost:8081\r\nUser-Agent: curl/7.64.0\r\nAccept: */*\r\n\r\n"
"POST /checkConfig HTTP/1.1\r\nHost: localhost:8081\r\nUser-Agent: curl/7.64.0\r\nAccept: */*\r\nContent-Type: application/json\r\nPostman-Token: f89b2256-392e-4b09-a307-362b79849852\r\ncache
-control: no-cache\r\nContent-Length: 384\r\n\r\n\n\n{\n  \"scalingActions\" : [\n    { \"deployment\" : \"euler-autoscaler-65bfd7279\"\n    , \"namespace\" : \"lambda\"\n    , \"rules\" : [\
n      { \"metric\" : \"custom_cpu\"\n      , \"desired\" : 50.00\n      },\n      { \"metric\" : \"istio_rps\"\n      , \"desired\" : 50.00\n      }\n    ]\n    , \"schedules\" : [\n      {
\"time\": \"2019-06-11T18:29:00Z\"\n      , \"min\" : 6\n      , \"max\" : 4\n      }\n    ]\n    }\n  ]\n}"
-}
--------------------------------------------------------------------------------

data Method = GET | POST deriving (Show)
data Protocol = HTTP | HTTPS | FTP deriving (Show)
data Request = Request
      {  method :: Method
        ,path   :: String
        ,routeParams :: [String]
        ,queryParam :: [(String,String)]
        ,protocol :: Protocol
        ,version :: String
        ,headers :: [(String , String)]
        ,body    :: String
      } deriving (Show)
--------------------------------------------------------------------------------
applicationJsonParser  = do
  body <- manyTill anyChar eof
  return body

urlEncodedParser = many $ do
  key <- try $ manyTill anyChar (char '=')
  value <- try $ manyTill anyChar (choice [(char '&'), (eof >> pure '|')])
  return ("\"" ++ key ++ "\" : \"" ++ value ++ "\" , ")

toMethod :: String -> Method
toMethod method = case toUpper <$> method  of
  "POST"       -> POST
  "GET"        -> GET
  otherwise    -> GET
toProtocol :: String -> Protocol
toProtocol protocol = case toUpper <$> protocol of
  "HTTPS"      -> HTTPS
  "HTTP"       -> HTTP
  "FTP"        -> FTP
  otherwise    -> HTTP

extractQueryParam = do
  path        <- many $ do
                 path1 <- try $ manyTill (noneOf ['=',':']) (char '/')
                 return $ path1 ++ "/"
  routeParams <- many $ do
                 _     <- try $ manyTill anyChar (char ':')
                 value <- try $ manyTill anyChar (choice [(char '/'),(eof >> pure '|')])
                 return value
  queryParams <- many $ do
                 key <-  try $ manyTill anyChar  (char '=')
                 value <- try $ manyTill anyChar (choice [(char '/'),(eof >> pure '|')])
                 return (key,value)
  return (path,routeParams,queryParams)

getPathAndParams  :: String -> ([String],[String],[(String,String)])
getPathAndParams path = case parse extractQueryParam "" path  of
    Left a -> ([""],[""],[("","")])
    Right b ->  b

getHeaders = many $ do
               header1 <- try $ manyTill (noneOf ['{']) (char ':')
               header2 <- try $ manyTill (noneOf ['{']) (string "\r\n")
               return (header1,header2)

requestParser :: Parsec String String Request
requestParser = do
 method   <- toMethod <$> manyTill anyChar space
 path     <- manyTill anyChar space
 protocol <- toProtocol <$> manyTill anyChar (char '/')
 version  <- manyTill anyChar (string "\r\n")
 headers  <- getHeaders
 _        <- try $ manyTill anyChar (string"\r\n")
 body     <- case find (\(x,y) -> x == "Content-Type") headers of
              Just (_, " application/json")                  -> applicationJsonParser
              Just (_, " application/x-www-form-urlencoded") -> do
                                                                res <- urlEncodedParser
                                                                return $ "{ " ++ (concat res )++ " }"
              Just (_,_)                                     -> applicationJsonParser
              Nothing                                        -> applicationJsonParser
 let pathAndParams = getPathAndParams path
 return $ Request { method = method
                   ,path   = concat $ fst' $ pathAndParams
                   ,routeParams = snd' $ pathAndParams
                   ,queryParam = trd' $ pathAndParams
                   ,protocol = protocol
                   ,version = version
                   ,headers = headers
                   ,body    = body
                  }
  where
     fst' (a,_,_) = a
     snd' (_,a,_) = a
     trd' (_,_,b) = b
