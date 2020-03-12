port module Main exposing (main)

import Audio
import Browser
import Browser.Dom exposing (getViewport)
import Browser.Events
    exposing
        ( onAnimationFrameDelta
        , onKeyDown
        , onKeyUp
        , onResize
        , onVisibilityChange
        )
import Components.Gamepad as Gamepad
import Html.Events exposing (keyCode)
import Json.Decode as Decode exposing (Value)
import Json.Encode as JE
import Messages exposing (Msg(..))
import Model exposing (Model)
import Ports exposing (gamepad)
import Task exposing (Task)
import View
import View.Font as Font
import View.Sprite as Sprite


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ onAnimationFrameDelta Animate
        , onKeyDown (Decode.map (KeyChange True) keyCode)
        , onKeyUp (Decode.map (KeyChange False) keyCode)
        , onResize Resize
        , onVisibilityChange VisibilityChange
        , gamepad (Gamepad.fromJson >> GamepadChange)
        ]


init : Value -> ( Model, Cmd Msg, Audio.AudioCmd Msg )
init _ =
    ( Model.initial
    , Cmd.batch
        [ Sprite.loadTexture TextureLoaded
        , Sprite.loadSprite SpriteLoaded
        , Font.load FontLoaded
        , Task.perform (\{ viewport } -> Resize (round viewport.width) (round viewport.height)) getViewport
        ]
    , Audio.cmdNone
    )


port toJS : JE.Value -> Cmd msg


port fromJS : (Decode.Value -> msg) -> Sub msg


main : Program Value (Audio.Model Msg Model) (Audio.Msg Msg)
main =
    Audio.elementWithAudio
        { init = init
        , view = View.view
        , subscriptions = subscriptions
        , update = Model.update
        , audio = always Audio.silence
        , audioPort = { toJS = toJS, fromJS = fromJS }
        }
