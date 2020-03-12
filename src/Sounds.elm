module Sounds exposing (..)

import Audio


type Sounds
    = Wall
    | Theme
    | Jump
    | Death
    | Action


soundCount : number
soundCount =
    5


type alias LoadedSounds =
    { wall : Audio.Source
    , theme : Audio.Source
    , jump : Audio.Source
    , death : Audio.Source
    , action : Audio.Source
    }
