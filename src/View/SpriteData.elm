module View.SpriteData exposing (sprite, spriteSrc, textureSrc, SpriteInfo)

import Dict exposing (Dict)


type alias SpriteInfo =
    { x : Float
    , y : Float
    , w : Float
    , h : Float
    }


sprite : Dict String SpriteInfo
sprite =
    Dict.fromList
        [
        ]


spriteSrc : String
spriteSrc =
    "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAIAAAABACAIAAABdtOgoAAAApUlEQVR4nO3RQREAIAzAMMC/501GHjQKetc7MyfO0wG/awDWAKwBWAOwBmANwBqANQBrANYArAFYA7AGYA3AGoA1AGsA1gCsAVgDsAZgDcAagDUAawDWAKwBWAOwBmANwBqANQBrANYArAFYA7AGYA3AGoA1AGsA1gCsAVgDsAZgDcAagDUAawDWAKwBWAOwBmANwBqANQBrANYArAFYA7AGYA3AFhvOA32iFHl+AAAAAElFTkSuQmCC"


textureSrc : String
textureSrc =
    "data:image/png;base64,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"
