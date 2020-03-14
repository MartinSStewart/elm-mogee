port module Main exposing (main)

import AssocList
import Audio exposing (Audio)
import Browser
import Browser.Dom exposing (getViewport)
import Browser.Events exposing (onAnimationFrame, onKeyDown, onKeyUp, onResize, onVisibilityChange)
import Components.Gamepad as Gamepad
import Duration
import Html.Events exposing (keyCode)
import Json.Decode as Decode
import Json.Encode as JE
import Messages exposing (BaseMsg(..), LoadingMsg_(..), Msg(..))
import Model exposing (BaseModel(..), GameState(..), LoadingModel_, Model)
import Ports exposing (gamepad)
import Quantity
import Sounds
import Task exposing (Task)
import View
import View.Font as Font
import View.Sprite as Sprite


subscriptions : BaseModel -> Sub BaseMsg
subscriptions baseModel =
    case baseModel of
        LoadingModel _ ->
            onResize Resize

        LoadedModel _ ->
            Sub.batch
                [ Sub.batch
                    [ onAnimationFrame Animate
                    , onKeyDown (Decode.map (KeyChange True) keyCode)
                    , onKeyUp (Decode.map (KeyChange False) keyCode)
                    , onVisibilityChange VisibilityChange
                    , gamepad (Gamepad.fromJson >> GamepadChange)
                    ]
                    |> Sub.map LoadedMsg
                , onResize Resize
                ]

        FailedToLoad ->
            Sub.none


init : Flags -> ( BaseModel, Cmd BaseMsg, Audio.AudioCmd BaseMsg )
init flags =
    ( LoadingModel
        { sounds = AssocList.empty
        , texture = Nothing
        , font = Nothing
        , sprite = Nothing
        , size = ( flags.windowWidth, flags.windowHeight )
        }
    , Cmd.batch
        [ Sprite.loadTexture TextureLoaded
        , Sprite.loadSprite SpriteLoaded
        , Font.load FontLoaded
        ]
        |> Cmd.map LoadingMsg
    , Audio.cmdBatch
        [ Audio.loadAudio (AudioLoaded Sounds.Wall) "snd/wall.wav"
        , Audio.loadAudio (AudioLoaded Sounds.Theme) "snd/theme.ogg"
        , Audio.loadAudio (AudioLoaded Sounds.Jump) "snd/jump.wav"
        , Audio.loadAudio (AudioLoaded Sounds.Death) "snd/death.ogg"
        , Audio.loadAudio (AudioLoaded Sounds.Action) "snd/action.wav"
        ]
        |> Audio.cmdMap LoadingMsg
    )


update : BaseMsg -> BaseModel -> ( BaseModel, Cmd BaseMsg, Audio.AudioCmd BaseMsg )
update msg baseModel =
    case ( msg, baseModel ) of
        ( LoadingMsg msg_, LoadingModel model ) ->
            let
                ( newModel, newCmd ) =
                    Model.updateLoading msg_ model
            in
            ( newModel, newCmd, Audio.cmdNone )

        ( LoadedMsg msg_, LoadedModel model ) ->
            ( Model.update msg_ model |> LoadedModel, Cmd.none, Audio.cmdNone )

        ( Resize width height, LoadingModel model ) ->
            ( LoadingModel { model | size = ( width, height ) }, Cmd.none, Audio.cmdNone )

        ( Resize width height, LoadedModel model ) ->
            ( LoadedModel { model | size = ( width, height ) }, Cmd.none, Audio.cmdNone )

        ( LoadingCompleted { sounds, texture, font, sprite } time, LoadingModel model ) ->
            ( Model.initial sounds texture font sprite model.size time |> LoadedModel
            , Cmd.none
            , Audio.cmdNone
            )

        _ ->
            ( baseModel, Cmd.none, Audio.cmdNone )


audio : BaseModel -> Audio
audio baseModel =
    let
        default =
            Audio.audioDefaultConfig
    in
    case baseModel of
        LoadingModel _ ->
            Audio.silence

        LoadedModel model ->
            if model.sound then
                case model.state of
                    Paused { playingStart, menu } ->
                        Audio.audioWithConfig
                            { default | loop = Just { loopStart = Quantity.zero, loopEnd = Duration.seconds 46.8 } }
                            model.soundData.theme
                            playingStart

                    Playing { playingStart } ->
                        Audio.audioWithConfig
                            { default | loop = Just { loopStart = Quantity.zero, loopEnd = Duration.seconds 46.8 } }
                            model.soundData.theme
                            playingStart

                    Dead { deathStart } ->
                        Audio.audio model.soundData.death deathStart

                    Initial menu ->
                        Audio.silence

            else
                Audio.silence

        FailedToLoad ->
            Audio.silence


port audioPortToJS : JE.Value -> Cmd msg


port audioPortFromJS : (Decode.Value -> msg) -> Sub msg


type alias Flags =
    { windowWidth : Int, windowHeight : Int }


main : Audio.Program Flags BaseModel BaseMsg
main =
    Audio.elementWithAudio
        { init = init
        , view = View.view
        , subscriptions = subscriptions
        , update = update
        , audio = audio
        , audioPort = { toJS = audioPortToJS, fromJS = audioPortFromJS }
        }
