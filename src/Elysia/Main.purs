module Elysia where

import Prelude
import Control.Monad.Error.Class (throwError)
import Control.Monad.State (StateT, get, put, execStateT) as ST
import Data.Symbol (class IsSymbol, reflectSymbol)
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (Error, message)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

type ElysiaApp r r'
  = { store :: { | r }, decorators :: { | r' } }

foreign import _newElysia :: forall r r'. Effect (ElysiaApp r r')

foreign import _listen :: forall r r'. Int -> ElysiaApp r r' -> ElysiaApp r r'

foreign import _state :: forall v r r'. String -> v -> ElysiaApp r r' -> ElysiaApp r' r'

foreign import _decorate :: forall v r r'. String -> v -> ElysiaApp r r' -> ElysiaApp r r'

foreign import _get :: forall r r'. String -> ({ params :: { id :: String }, store :: { | r }, decorators :: { | r' } } -> String) -> ElysiaApp r r' -> ElysiaApp r r'

state :: forall k v r r'. IsSymbol k => Proxy k -> v -> ST.StateT (ElysiaApp r r') Effect Unit
state proxy value = do
  app <- ST.get
  let
    store' = unsafeCoerce (_state (reflectSymbol proxy) value app)
  ST.put store'

decorate :: forall k v r r'. IsSymbol k => Proxy k -> v -> ST.StateT (ElysiaApp r r') Effect Unit
decorate proxy value = do
  app <- ST.get
  let
    decorators' = unsafeCoerce (_decorate (reflectSymbol proxy) value app)
  ST.put decorators'

get :: forall r r'. String -> ({ params :: { id :: String }, store :: { | r }, decorators :: { | r' } } -> String) -> ST.StateT (ElysiaApp r r') Effect Unit
get route handler = do
  app <- ST.get
  let
    app' = unsafeCoerce (_get route handler app)
  ST.put app'

listen :: forall r r'. Int -> ST.StateT (ElysiaApp r r') Effect Unit -> Effect (ElysiaApp r r')
listen port appM = do
  app <- ST.execStateT appM =<< _newElysia
  pure $ _listen port app
