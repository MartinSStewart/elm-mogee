module Messages exposing (BaseMsg(..), Msg(..))

import Audio
import Browser.Events exposing (Visibility(..))
import Components.Gamepad exposing (Gamepad)
import Sounds
import WebGL.Texture exposing (Error, Texture)


type Msg
    = Resize Int Int
    | Animate Float
    | KeyChange Bool Int
    | GamepadChange Gamepad
    | VisibilityChange Visibility
    | TextureLoaded (Result Error Texture)
    | SpriteLoaded (Result Error Texture)
    | FontLoaded (Result Error Texture)


type BaseMsg
    = AudioLoaded Sounds.Sounds (Result Audio.LoadError Audio.Source)
    | SoundsLoadedMsg Msg
