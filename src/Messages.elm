module Messages exposing (BaseMsg(..), LoadingMsg_(..), Msg(..))

import Audio
import Browser.Events exposing (Visibility(..))
import Components.Gamepad exposing (Gamepad)
import Sounds
import Time
import WebGL.Texture exposing (Error, Texture)


type Msg
    = Animate Time.Posix
    | KeyChange Bool Int
    | GamepadChange Gamepad
    | VisibilityChange Visibility


type LoadingMsg_
    = AudioLoaded Sounds.Sounds (Result Audio.LoadError Audio.Source)
    | TextureLoaded (Result Error Texture)
    | SpriteLoaded (Result Error Texture)
    | FontLoaded (Result Error Texture)


type BaseMsg
    = LoadingMsg LoadingMsg_
    | LoadedMsg Msg
    | Resize Int Int
    | LoadingCompleted { sounds : Sounds.LoadedSounds, texture : Texture, font : Texture, sprite : Texture } Time.Posix
