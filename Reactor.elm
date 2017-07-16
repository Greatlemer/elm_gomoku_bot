module Reactor exposing (..)

import App exposing (init, Model, Msg, subscriptions, update, view)
import Html

-- Main
-- Calls through to the main app setting flags so we can use it with reactor.
main : Program Never Model Msg
main =
  Html.program
    { init = init {
        serverAddress = "ws://hub.nechifor.net/"
      }
    , view = view
    , update = update
    , subscriptions = subscriptions
    }