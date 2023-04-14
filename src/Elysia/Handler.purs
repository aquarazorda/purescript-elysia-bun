module Elysia.Handler
  ( HandlerM(..)
  , Handler()
  , runHandlerM
  ) where

import Prelude
import Effect (Effect)
import Effect.Aff (Aff, runAff_)
import Data.Either (either)
import Elysia.Types (Context)
import Control.Monad.Reader.Class (class MonadAsk)

-- | Monad responsible for handling single request.
newtype HandlerM r a
  = HandlerM (Context r -> Aff a)

type Handler r
  = HandlerM r Unit

instance functorHandlerM :: Functor (HandlerM r) where
  map f (HandlerM h) =
    HandlerM \ctx ->
      (h ctx >>= \r -> pure $ f r)

instance applyHandlerM :: Apply (HandlerM r) where
  apply (HandlerM f) (HandlerM h) =
    HandlerM \ctx -> do
      trans <- f ctx
      res <- h ctx
      pure $ trans res

instance applicativeHandlerM :: Applicative (HandlerM r) where
  pure x = HandlerM \_ -> pure x

instance bindHandlerM :: Bind (HandlerM r) where
  bind (HandlerM h) f =
    HandlerM \ctx -> do
      (HandlerM g) <- liftM1 f $ h ctx
      g ctx

instance monadHandlerM :: Monad (HandlerM r)

instance monadAskHandlerM :: MonadAsk (Context r) (HandlerM r) where
  ask = HandlerM \ctx -> pure ctx

runHandlerM :: forall r. Handler r -> Context r -> Effect Unit -> Effect Unit
runHandlerM (HandlerM h) ctx nxt = void $ runAff_ (either (const nxt) pure) (h ctx)
