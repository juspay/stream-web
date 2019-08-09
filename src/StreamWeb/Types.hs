module StreamWeb.Types
  ( Request  (..)
  , Method   (..)
  , Protocol (..)
  ) where

import GHC.Generics
import GHC.Show
import Data.ByteString
import Control.Monad.State
import qualified Data.HashMap.Lazy as Map
import qualified Data.ByteString.Char8 as BS

type Action m = Request -> m ByteString
type Header = (ByteString, ByteString)
type RoutePattern = String

newtype StreamW e m a = StreamW {runS :: State (StreamState e m) a}
newtype ErrorHandler e m = ErrorHandler {runError :: e -> m ()}

data StreamState e m = StreamState
                       { routes :: [Route m]
                       , middlewares :: [Action m]
                       , handler :: ErrorHandler e m
                       }
data Route m = Route
               { methodR :: Method
               , pat :: RoutePattern
               , action :: Request -> m ByteString
               }

data Method =
     OPTIONS |
     GET     |
     POST    deriving (Show, Eq)

data Protocol =
     HTTP  |
     HTTPS |
     FTP deriving (Show)

data Request = Request
      { method   :: Method
      , path     :: BS.ByteString
      , queryParams :: Map.HashMap BS.ByteString BS.ByteString
      , protocol :: Protocol
      , version  :: BS.ByteString
      , headers  :: [(BS.ByteString , BS.ByteString)]
      , body     :: BS.ByteString
      } deriving (Show)



