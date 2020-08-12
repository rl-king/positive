module Generated.Data.ImageSettings exposing (ImageSettings, decodeCoordinate, encodeCoordinate)

import Json.Decode
import Json.Decode.Pipeline
import Json.Encode


type alias ImageSettings =
    { isPath : String
    , isGamma : Float
    , isZone1 : Float
    , isZone5 : Float
    , isZone9 : Float
    , isBlackpoint : Float
    , isWhitepoint : Float
    }


encodeCoordinate : ImageSettings -> Json.Encode.Value
encodeCoordinate a =
    Json.Encode.object
        [ ( "isPath", Json.Encode.string a.isPath )
        , ( "isGamma", Json.Encode.float a.isGamma )
        , ( "isZone1", Json.Encode.float a.isZone1 )
        , ( "isZone5", Json.Encode.float a.isZone5 )
        , ( "isZone9", Json.Encode.float a.isZone9 )
        , ( "isBlackpoint", Json.Encode.float a.isBlackpoint )
        , ( "isWhitepoint", Json.Encode.float a.isWhitepoint )
        ]


decodeCoordinate : Json.Decode.Decoder ImageSettings
decodeCoordinate =
    Json.Decode.succeed ImageSettings
        |> Json.Decode.Pipeline.required "isPath" Json.Decode.string
        |> Json.Decode.Pipeline.required "isGamma" Json.Decode.float
        |> Json.Decode.Pipeline.required "isZone1" Json.Decode.float
        |> Json.Decode.Pipeline.required "isZone5" Json.Decode.float
        |> Json.Decode.Pipeline.required "isZone9" Json.Decode.float
        |> Json.Decode.Pipeline.required "isBlackpoint" Json.Decode.float
        |> Json.Decode.Pipeline.required "isWhitepoint" Json.Decode.float
