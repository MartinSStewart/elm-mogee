module Model exposing
    ( BaseModel(..)
    , GameState(..)
    , LoadingModel_
    , Model
    , initial
    , update
    , updateLoading
    )

import AssocList
import Audio
import Browser.Events exposing (Visibility(..))
import Components.Components as Components exposing (Components)
import Components.Gamepad as Gamepad
import Components.Keys as Keys exposing (Keys, codes)
import Components.Menu as Menu exposing (Menu)
import Maybe exposing (Maybe(..))
import Messages exposing (BaseMsg(..), LoadingMsg_(..), Msg(..))
import Slides.Engine as Engine exposing (Engine)
import Slides.Slides as Slides
import Sounds exposing (Sounds)
import Systems.Mogee as Mogee
import Systems.Systems as Systems exposing (Systems)
import Task
import Time
import WebGL.Texture exposing (Error, Texture)


type GameState
    = Paused { playingStart : Time.Posix, menu : Menu }
    | Playing { playingStart : Time.Posix }
    | Dead { deathStart : Time.Posix }
    | Initial Menu


type alias Model =
    { systems : Systems
    , components : Components
    , time : Time.Posix
    , state : GameState
    , lives : Int
    , score : Int
    , size : ( Int, Int )
    , padding : Int
    , sound : Bool
    , texture : Texture
    , sprite : Texture
    , font : Texture
    , keys : Keys
    , slides : Engine
    , soundData : Sounds.LoadedSounds
    , menuLastAction : Maybe Time.Posix
    , lastWallHit : Maybe Time.Posix
    , lastJump : Maybe Time.Posix
    }


type alias LoadingModel_ =
    { sounds : AssocList.Dict Sounds Audio.Source
    , texture : Maybe Texture
    , sprite : Maybe Texture
    , font : Maybe Texture
    , size : ( Int, Int )
    }


type BaseModel
    = LoadingModel LoadingModel_
    | LoadedModel Model
    | FailedToLoad


initial : Sounds.LoadedSounds -> Texture -> Texture -> Texture -> ( Int, Int ) -> Time.Posix -> Model
initial loadedSounds texture font sprite size time =
    { components = Components.initial
    , systems = Systems.initial
    , time = time
    , lives = 0
    , score = 0
    , state = Initial Menu.start
    , size = size
    , padding = 0
    , sound = True
    , texture = texture
    , font = font
    , sprite = sprite
    , keys = Keys.initial
    , slides = Slides.initial
    , soundData = loadedSounds
    , menuLastAction = Nothing
    , lastWallHit = Nothing
    , lastJump = Nothing
    }


updateLoading : LoadingMsg_ -> LoadingModel_ -> ( BaseModel, Cmd BaseMsg )
updateLoading msg model =
    case msg of
        AudioLoaded sound result ->
            case result of
                Ok audioSource ->
                    { model | sounds = AssocList.insert sound audioSource model.sounds }
                        |> handleLoadingComplete

                Err _ ->
                    ( FailedToLoad, Cmd.none )

        TextureLoaded result ->
            case result of
                Ok texture ->
                    { model | texture = Just texture } |> handleLoadingComplete

                Err _ ->
                    ( FailedToLoad, Cmd.none )

        SpriteLoaded result ->
            case result of
                Ok sprite ->
                    { model | sprite = Just sprite } |> handleLoadingComplete

                Err _ ->
                    ( FailedToLoad, Cmd.none )

        FontLoaded result ->
            case result of
                Ok font ->
                    { model | font = Just font } |> handleLoadingComplete

                Err _ ->
                    ( FailedToLoad, Cmd.none )


handleLoadingComplete : LoadingModel_ -> ( BaseModel, Cmd BaseMsg )
handleLoadingComplete model =
    let
        map : Sounds.Sounds -> Maybe (Audio.Source -> b) -> Maybe b
        map sound_ constructor =
            case AssocList.get sound_ model.sounds of
                Just audioSource_ ->
                    constructor |> Maybe.andThen (\c -> c audioSource_ |> Just)

                Nothing ->
                    Nothing
    in
    case
        ( map Sounds.Wall (Just Sounds.LoadedSounds)
            |> map Sounds.Theme
            |> map Sounds.Jump
            |> map Sounds.Death
            |> map Sounds.Action
        , model.texture
        , ( model.font, model.sprite )
        )
    of
        ( Just allSounds, Just texture, ( Just font, Just sprite ) ) ->
            ( LoadingModel model
            , Task.perform
                (LoadingCompleted { sounds = allSounds, texture = texture, font = font, sprite = sprite })
                Time.now
            )

        _ ->
            ( LoadingModel model, Cmd.none )


update : Msg -> Model -> Model
update action model =
    case action of
        Animate time ->
            let
                elapsed =
                    Time.posixToMillis time - Time.posixToMillis model.time |> toFloat
            in
            { model | time = time }
                |> animate elapsed
                |> animateKeys elapsed

        KeyChange pressed keyCode ->
            { model
                | keys = Keys.keyChange pressed keyCode model.keys
                , padding =
                    -- resize the vieport with `-` and `=`
                    case ( pressed, keyCode ) of
                        ( True, 189 ) ->
                            model.padding + 1

                        ( True, 187 ) ->
                            max 0 (model.padding - 1)

                        _ ->
                            model.padding
            }

        GamepadChange gamepad_ ->
            { model | keys = Keys.gamepadChange gamepad_ model.keys }

        VisibilityChange Visible ->
            model

        VisibilityChange Hidden ->
            { model
                | state =
                    case model.state of
                        Playing { playingStart } ->
                            Paused { playingStart = playingStart, menu = Menu.paused }

                        _ ->
                            model.state
            }


animate : Float -> Model -> Model
animate elapsed model =
    case model.state of
        Initial menu ->
            updateMenu elapsed Initial model.time menu model

        Paused paused ->
            updateMenu
                elapsed
                (\menu -> Paused { playingStart = paused.playingStart, menu = menu })
                paused.playingStart
                paused.menu
                model

        Playing { playingStart } ->
            let
                limitElapsed =
                    min elapsed 60

                ( newComponents, newSystems, soundEvent ) =
                    Systems.run
                        limitElapsed
                        (Keys.directions model.keys)
                        model.components
                        model.systems

                state =
                    if Keys.pressed codes.escape model.keys || Keys.pressed codes.q model.keys then
                        Paused { playingStart = playingStart, menu = Menu.paused }

                    else
                        model.state

                newState =
                    checkLives
                        { model
                            | components = newComponents
                            , systems = newSystems
                            , state = state
                            , lastWallHit =
                                case soundEvent of
                                    Just Mogee.Wall ->
                                        Just model.time

                                    _ ->
                                        model.lastWallHit
                            , lastJump =
                                case soundEvent of
                                    Just Mogee.Jump ->
                                        Just model.time

                                    _ ->
                                        model.lastJump
                        }
            in
            newState

        Dead _ ->
            if Keys.pressed codes.enter model.keys then
                if model.lives == 0 then
                    { model | state = Initial Menu.start }

                else
                    continue model

            else
                model


animateKeys : Float -> Model -> Model
animateKeys elapsed model =
    { model | keys = Keys.animate elapsed model.keys }


checkLives : Model -> Model
checkLives model =
    if Components.isDead model.components then
        { model
            | lives = model.lives - 1
            , state = Dead { deathStart = model.time }
        }

    else
        model


continue : Model -> Model
continue model =
    { model
        | state = Playing { playingStart = model.time }
        , components = Components.initial
        , systems = Systems.initial
        , score = model.score + model.systems.currentScore
    }


start : Model -> Model
start model =
    { model
        | state = Playing { playingStart = model.time }
        , components = Components.initial
        , systems = Systems.initial
        , lives = 3
        , score = 0
    }


updateMenu : Float -> (Menu -> GameState) -> Time.Posix -> Menu -> Model -> Model
updateMenu elapsed menuState playingStart menu model =
    let
        ( newMenu, cmd ) =
            Menu.update elapsed model.sound model.keys menu

        newModel =
            if menu.section == Menu.SlidesSection then
                { model | slides = Engine.update elapsed model.keys model.slides }

            else
                model
    in
    case cmd of
        Menu.Start ->
            start { newModel | state = Initial newMenu, menuLastAction = Just model.time }

        Menu.ToggleSound on ->
            { newModel | sound = on, state = Initial newMenu, menuLastAction = Just model.time }

        Menu.Resume ->
            { newModel | state = Playing { playingStart = playingStart }, menuLastAction = Just model.time }

        Menu.End ->
            { newModel | state = Initial Menu.start, menuLastAction = Just model.time }

        Menu.Action ->
            { newModel | state = menuState newMenu, menuLastAction = Just model.time }

        Menu.Noop ->
            { newModel | state = menuState newMenu }
