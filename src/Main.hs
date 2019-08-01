module Main where

import StreamWeb (startServer, sendJson)
import qualified Streamly.Prelude as SP
import qualified Streamly as S

main :: IO ()
main = SP.drain $ S.parallely $ SP.mapM (\(so, req) -> sendJson so req "OKDONE") $ startServer 8081
