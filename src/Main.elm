port module Main exposing (main)

import AssocList
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
import Messages exposing (BaseMsg(..), Msg(..))
import Model exposing (BaseModel(..), Model)
import Ports exposing (gamepad)
import Sounds
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


init : Value -> ( BaseModel, Cmd BaseMsg, Audio.AudioCmd BaseMsg )
init _ =
    ( SoundsLoading AssocList.empty
    , Cmd.batch
        [ Sprite.loadTexture TextureLoaded
        , Sprite.loadSprite SpriteLoaded
        , Font.load FontLoaded
        , Task.perform (\{ viewport } -> Resize (round viewport.width) (round viewport.height)) getViewport
        ]
    , Audio.cmdBatch
        [ Audio.loadAudio AudioLoaded "assets/sound.ogg"
        ]
    )


update : BaseMsg -> BaseModel -> ( BaseModel, Cmd BaseMsg, Audio.AudioCmd BaseMsg )
update msg baseModel =
    case ( msg, baseModel ) of
        ( AudioLoaded sound result, SoundsLoading dict ) ->
            case result of
                Ok audioSource ->
                    let
                        newDict =
                            AssocList.insert sound audioSource dict

                        map : Sounds.Sounds -> Maybe (a -> b) -> Maybe b
                        map sound_ constructor =
                            case AssocList.get sound_ newDict of
                                Just audioSource_ ->
                                    constructor |> Maybe.andThen (\c -> c audioSource_ |> Just)

                                Nothing ->
                                    Nothing
                    in
                    case
                        map Sounds.Wall (Just Sounds.LoadedSounds)
                            |> map Sounds.Theme
                            |> map Sounds.Jump
                            |> map Sounds.Death
                            |> map Sounds.Action
                    of
                        Just allSounds ->
                            ( Model.initial allSounds |> SoundsLoaded, Cmd.none, Audio.cmdNone )

                        Nothing ->
                            ( SoundsLoading newDict, Cmd.none, Audio.cmdNone )

                Err _ ->
                    ( SoundsDidNotLoad, Cmd.none, Audio.cmdNone )

        ( SoundsLoadedMsg msg_, SoundsLoaded model ) ->
            let
                ( newModel, newCmd ) =
                    Model.update msg_ model
            in
            ( SoundsLoaded newModel, Cmd.map SoundsLoadedMsg newCmd, Audio.cmdNone )

        ( _, SoundsDidNotLoad ) ->
            ( SoundsDidNotLoad, Cmd.none, Audio.cmdNone )


port toJS : JE.Value -> Cmd msg


port fromJS : (Decode.Value -> msg) -> Sub msg


main : Program Value (Audio.Model Msg BaseModel) (Audio.Msg Msg)
main =
    Audio.elementWithAudio
        { init = init
        , view = View.view
        , subscriptions = subscriptions
        , update = Model.update
        , audio = always Audio.silence
        , audioPort = { toJS = toJS, fromJS = fromJS }
        }
