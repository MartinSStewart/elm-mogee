port module Ports exposing (gamepad)

import Json.Decode exposing (Value)


{-| port for handling gamepad
-}
port gamepad : (Value -> msg) -> Sub msg
