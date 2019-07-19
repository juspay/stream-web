{-# LANGUAGE ScopedTypeVariables #-}

module Utils where

import Types
import Control.Monad.State

addRoute :: forall e m a. Method -> RoutePattern -> Action m -> StreamW e m ()
addRoute method pat action = StreamW $ modify $ \s -> s { routes = Route method pat action : routes s}
