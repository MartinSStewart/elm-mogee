module View.FontData exposing (font, fontSrc, CharInfo)

import Dict exposing (Dict)


type alias CharInfo =
    { x : Float
    , y : Float
    , w : Float
    }


font : Dict String CharInfo
font =
    Dict.fromList
        [ ( "!", CharInfo 0 0 1 )
        , ( "\"", CharInfo 1 0 3 )
        , ( "#", CharInfo 4 0 5 )
        , ( "$", CharInfo 9 0 4 )
        , ( "%", CharInfo 13 0 4 )
        , ( "&", CharInfo 17 0 6 )
        , ( "'", CharInfo 23 0 1 )
        , ( "(", CharInfo 24 0 3 )
        , ( ")", CharInfo 27 0 3 )
        , ( "*", CharInfo 30 0 5 )
        , ( "+", CharInfo 35 0 5 )
        , ( ",", CharInfo 40 0 1 )
        , ( "-", CharInfo 41 0 4 )
        , ( ".", CharInfo 45 0 1 )
        , ( "/", CharInfo 46 0 3 )
        , ( "0", CharInfo 49 0 4 )
        , ( "1", CharInfo 53 0 4 )
        , ( "2", CharInfo 57 0 4 )
        , ( "3", CharInfo 61 0 4 )
        , ( "4", CharInfo 65 0 4 )
        , ( "5", CharInfo 69 0 4 )
        , ( "6", CharInfo 73 0 4 )
        , ( "7", CharInfo 77 0 4 )
        , ( "8", CharInfo 81 0 4 )
        , ( "9", CharInfo 85 0 4 )
        , ( ":", CharInfo 89 0 1 )
        , ( ";", CharInfo 90 0 2 )
        , ( "<", CharInfo 92 0 5 )
        , ( "=", CharInfo 97 0 4 )
        , ( ">", CharInfo 101 0 5 )
        , ( "?", CharInfo 106 0 4 )
        , ( "@", CharInfo 110 0 7 )
        , ( "A", CharInfo 117 0 4 )
        , ( "B", CharInfo 121 0 4 )
        , ( "C", CharInfo 0 11 4 )
        , ( "D", CharInfo 4 11 4 )
        , ( "E", CharInfo 8 11 3 )
        , ( "F", CharInfo 11 11 3 )
        , ( "G", CharInfo 14 11 4 )
        , ( "H", CharInfo 18 11 4 )
        , ( "I", CharInfo 22 11 3 )
        , ( "J", CharInfo 25 11 4 )
        , ( "K", CharInfo 29 11 4 )
        , ( "L", CharInfo 33 11 3 )
        , ( "M", CharInfo 36 11 5 )
        , ( "N", CharInfo 41 11 5 )
        , ( "O", CharInfo 46 11 4 )
        , ( "P", CharInfo 50 11 4 )
        , ( "Q", CharInfo 54 11 4 )
        , ( "R", CharInfo 58 11 4 )
        , ( "S", CharInfo 62 11 4 )
        , ( "T", CharInfo 66 11 5 )
        , ( "U", CharInfo 71 11 4 )
        , ( "V", CharInfo 75 11 4 )
        , ( "W", CharInfo 79 11 7 )
        , ( "X", CharInfo 86 11 5 )
        , ( "Y", CharInfo 91 11 5 )
        , ( "Z", CharInfo 96 11 4 )
        , ( "[", CharInfo 100 11 2 )
        , ( "\\", CharInfo 102 11 3 )
        , ( "]", CharInfo 105 11 2 )
        , ( "^", CharInfo 107 11 3 )
        , ( "_", CharInfo 110 11 4 )
        , ( "a", CharInfo 114 11 3 )
        , ( "b", CharInfo 117 11 3 )
        , ( "c", CharInfo 120 11 3 )
        , ( "d", CharInfo 123 11 3 )
        , ( "e", CharInfo 0 22 3 )
        , ( "f", CharInfo 3 22 4 )
        , ( "ff", CharInfo 7 22 6 )
        , ( "ffi", CharInfo 13 22 6 )
        , ( "fi", CharInfo 19 22 4 )
        , ( "fj", CharInfo 23 22 4 )
        , ( "g", CharInfo 27 22 3 )
        , ( "gj", CharInfo 30 22 5 )
        , ( "h", CharInfo 35 22 3 )
        , ( "i", CharInfo 38 22 1 )
        , ( "j", CharInfo 39 22 3 )
        , ( "jj", CharInfo 42 22 5 )
        , ( "k", CharInfo 47 22 3 )
        , ( "l", CharInfo 50 22 1 )
        , ( "m", CharInfo 51 22 5 )
        , ( "n", CharInfo 56 22 3 )
        , ( "o", CharInfo 59 22 3 )
        , ( "p", CharInfo 62 22 3 )
        , ( "q", CharInfo 65 22 3 )
        , ( "r", CharInfo 68 22 3 )
        , ( "s", CharInfo 71 22 3 )
        , ( "ss", CharInfo 74 22 6 )
        , ( "t", CharInfo 80 22 3 )
        , ( "u", CharInfo 83 22 3 )
        , ( "v", CharInfo 86 22 3 )
        , ( "w", CharInfo 89 22 5 )
        , ( "x", CharInfo 94 22 3 )
        , ( "y", CharInfo 97 22 3 )
        , ( "yj", CharInfo 100 22 5 )
        , ( "z", CharInfo 105 22 3 )
        , ( "{", CharInfo 108 22 3 )
        , ( "|", CharInfo 111 22 1 )
        , ( "}", CharInfo 112 22 3 )
        , ( "~", CharInfo 115 22 4 )
        , ( "×", CharInfo 119 22 5 )
        , ( "⁄", CharInfo 124 22 3 )
        ]


fontSrc : String
fontSrc =
    "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAIAAAABAAQAAAAD6rULSAAABzklEQVR4nO2RTUjTcRyHn/9vf9oLDMcqG73gqCXK0LqEI6wW5EiFbgUdhF0SJI3Ag/S2/TV6UShG8+AhGJ0MoroEO0Rzu4vtEi2EXBG4ZDZxy0XL/7dDK8ku3es5Pqfnw4enJVOisoFuTFklT53C9rjqXRr4mKdh/N1xO1QaHqq1CZvspvnAcNQfvdxUQHfE5YlXIkfWs55Hfefbc2ptusC52/r89emBU5dOvmhRE0dvsmuE4qxY1K2sBXXjtaOTq47uoZj9UPliIIcsSLIeUhJTBBF5uZHVJJrwO4rNjCrPcMfhUIjZN3OWRK2m9Vvbzp54b7Cn0fl5x6oP5SIoV1wQRFtcqcwFFJMtYoFlAy2d2dbq0Rls06DoPoiEamS9agUyYzaPwnzuzRiuvPI/SE1l7ZmZzFCOGO5O6okzIiLVhR/pvygnNgn5duzPLfrXx+Ut7Yt+LwDXLBFlJt/m9/YAGBAJwpgNd9ez0eatPnvrfkdqWX91+ksF7PvM5N004eoFlQYgjXYvrxnFxrAKg9MgiOpGjA+V+8oLVcCaiHcM+rIS06r9O+edZ3rMVRd8utMVUMD4zyhTpW1U+0Z8qV5ZL4nIpCl/ccN/8Q+J738J8j8zAZdJAAAAAElFTkSuQmCC"
