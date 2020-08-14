module Generated.Data.ImageSettings exposing (ImageSettings, decodeImageSettings, encodeImageSettings)

import Json.Decode
import Json.Decode.Pipeline
import Json.Encode


type alias ImageSettings =
    { iFilename : String
    , iRotate : Float
    , iCrop : Int
    , iGamma : Float
    , iZone1 : Float
    , iZone5 : Float
    , iZone9 : Float
    , iBlackpoint : Float
    , iWhitepoint : Float
    }


encodeImageSettings : ImageSettings -> Json.Encode.Value
encodeImageSettings a =
    Json.Encode.object
        [ ( "iFilename", Json.Encode.string a.iFilename )
        , ( "iRotate", Json.Encode.float a.iRotate )
        , ( "iCrop", Json.Encode.int a.iCrop )
        , ( "iGamma", Json.Encode.float a.iGamma )
        , ( "iZone1", Json.Encode.float a.iZone1 )
        , ( "iZone5", Json.Encode.float a.iZone5 )
        , ( "iZone9", Json.Encode.float a.iZone9 )
        , ( "iBlackpoint", Json.Encode.float a.iBlackpoint )
        , ( "iWhitepoint", Json.Encode.float a.iWhitepoint )
        ]


decodeImageSettings : Json.Decode.Decoder ImageSettings
decodeImageSettings =
    Json.Decode.succeed ImageSettings
        |> Json.Decode.Pipeline.required "iFilename" Json.Decode.string
        |> Json.Decode.Pipeline.required "iRotate" Json.Decode.float
        |> Json.Decode.Pipeline.required "iCrop" Json.Decode.int
        |> Json.Decode.Pipeline.required "iGamma" Json.Decode.float
        |> Json.Decode.Pipeline.required "iZone1" Json.Decode.float
        |> Json.Decode.Pipeline.required "iZone5" Json.Decode.float
        |> Json.Decode.Pipeline.required "iZone9" Json.Decode.float
        |> Json.Decode.Pipeline.required "iBlackpoint" Json.Decode.float
        |> Json.Decode.Pipeline.required "iWhitepoint" Json.Decode.float
