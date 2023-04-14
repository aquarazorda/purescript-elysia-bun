module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Elysia (get, listen, state)
import Elysia.Handler (HandlerM(..))
import Type.Proxy (Proxy(..))

-- pingHandler =
--   HandlerM \ctx _ -> do
--     pure "pong"
main :: Effect Unit
main = do
  let
    appM = do
      state (Proxy :: Proxy "build") 1
      get "/ayo" (\{ params, store } -> show $ store.build + 2)
  _ <- listen 8080 appM
  log "Listening on port 8080"
