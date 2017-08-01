module View.Font
    exposing
        ( textMesh
        , renderText
        , Vertex
        )

import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import WebGL exposing (Texture, Shader, Mesh, Entity)
import WebGL.Texture as Texture
import Dict exposing (Dict)
import String


type alias CharInfo =
    { x : Float
    , y : Float
    , w : Float
    }


fontHeight : Float
fontHeight =
    11


spaceWidth : Float
spaceWidth =
    3


font : Dict Char CharInfo
font =
    Dict.fromList
        [ ( 'a', CharInfo 0 0 3 )
        , ( 'e', CharInfo 3 0 3 )
        , ( 'l', CharInfo 6 0 1 )
        , ( 'o', CharInfo 7 0 3 )
        , ( 'p', CharInfo 10 0 3 )
        , ( 'r', CharInfo 13 0 3 )
        , ( 's', CharInfo 16 0 3 )
        , ( 't', CharInfo 19 0 3 )
        , ( 'y', CharInfo 22 0 3 )
        ]


kerning : Dict ( Char, Char ) Float
kerning =
    Dict.fromList
        [ ( ( 't', 'o' ), 0 )
        ]


type alias Vertex =
    { position : Vec2
    , texPosition : Vec2
    }


renderText : ( Float, Float, Float ) -> Mesh Vertex -> Texture -> ( Float, Float, Float ) -> Entity
renderText color mesh texture offset =
    WebGL.entity
        texturedVertexShader
        texturedFragmentShader
        mesh
        { offset = Vec3.fromTuple offset
        , texture = texture
        , color = Vec3.fromTuple color
        , textureOffset = vec2 0 0
        , textureSize = vec2 (toFloat (Tuple.first (Texture.size texture))) (toFloat (Tuple.second (Texture.size texture)))
        }


textMesh : String -> Mesh Vertex
textMesh text =
    WebGL.triangles (textMeshHelper Nothing text 0 0 [])


textMeshHelper : Maybe Char -> String -> Float -> Float -> List ( Vertex, Vertex, Vertex ) -> List ( Vertex, Vertex, Vertex )
textMeshHelper prevChar text currentX currentY list =
    case String.uncons text of
        Just ( ' ', rest ) ->
            textMeshHelper (Just ' ') rest (currentX + spaceWidth) currentY list

        Just ( '\n', rest ) ->
            textMeshHelper (Just '\n') rest 0 (currentY + fontHeight) list

        Just ( char, rest ) ->
            case Dict.get char font of
                Just charInfo ->
                    addLetter charInfo ( currentX + (letterSpacing prevChar char), currentY ) <|
                        textMeshHelper (Just char) rest (currentX + charInfo.w + (letterSpacing prevChar char)) currentY list

                Nothing ->
                    textMeshHelper prevChar rest currentX currentY list

        Nothing ->
            list


letterSpacing : Maybe Char -> Char -> Float
letterSpacing prevChar nextChar =
    case prevChar of
        Nothing ->
            0

        Just ' ' ->
            0

        Just '\n' ->
            0

        Just char ->
            Dict.get ( char, nextChar ) kerning
                |> Maybe.withDefault 1


addLetter : CharInfo -> ( Float, Float ) -> List ( Vertex, Vertex, Vertex ) -> List ( Vertex, Vertex, Vertex )
addLetter char ( x, y ) =
    [ ( Vertex (vec2 x y) (vec2 char.x char.y)
      , Vertex (vec2 (x + char.w) (y + fontHeight)) (vec2 (char.x + char.w) (char.y + fontHeight))
      , Vertex (vec2 (x + char.w) y) (vec2 (char.x + char.w) char.y)
      )
    , ( Vertex (vec2 x y) (vec2 char.x char.y)
      , Vertex (vec2 x (y + fontHeight)) (vec2 char.x (char.y + fontHeight))
      , Vertex (vec2 (x + char.w) (y + fontHeight)) (vec2 (char.x + char.w) (char.y + fontHeight))
      )
    ]
        |> (++)



-- Shaders


type alias Uniform =
    { offset : Vec3
    , color : Vec3
    , texture : Texture
    , textureSize : Vec2
    , textureOffset : Vec2
    }


type alias Varying =
    { texturePos : Vec2 }


texturedVertexShader : Shader Vertex Uniform Varying
texturedVertexShader =
    [glsl|

        precision mediump float;
        attribute vec2 position;
        attribute vec2 texPosition;
        uniform vec3 offset;
        varying vec2 texturePos;

        void main () {
            vec2 roundOffset = vec2(floor(offset.x + 0.5), floor(offset.y + 0.5));
            vec2 clipSpace = position + roundOffset - 32.0;
            gl_Position = vec4(clipSpace.x, -clipSpace.y, offset.z, 32.0);
            texturePos = texPosition;
        }

    |]


texturedFragmentShader : Shader {} Uniform Varying
texturedFragmentShader =
    [glsl|

        precision mediump float;
        uniform sampler2D texture;
        uniform vec2 textureSize;
        uniform vec2 textureOffset;
        uniform vec3 color;
        varying vec2 texturePos;

        void main () {
          vec2 offset = (texturePos + textureOffset) / textureSize;
          vec4 textureColor = texture2D(texture, offset);
          gl_FragColor = vec4(color, 1.0);
          if (textureColor.r == 1.0) discard;
        }

    |]
