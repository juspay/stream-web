{-# LANGUAGE DeriveGeneric #-}
module Types where

import GHC.Generics
import GHC.Show
import Data.ByteString
import Control.Monad.State

type Action m = Request -> m ByteString
newtype StreamW e m a = StreamW {runS :: State (StreamState e m) a}
data StreamState e m = StreamState
                       { routes :: [Route m]
                       , middlewares :: [Action m]
                       , handler :: ErrorHandler e m
                       }
newtype ErrorHandler e m = ErrorHandler {runError :: e -> m ()}

data Route m = Route
               { methodR :: Method
               , pat :: RoutePattern
               , action :: Request -> m ByteString
               }

data Request = Request
               { method  :: Method
               , path    :: String
               , body    :: ByteString
               , headers :: [Header]
               } deriving (Show, Generic)

data Method = GET | POST deriving (Show, Generic)

type Header = (ByteString, ByteString)

type RoutePattern = String


