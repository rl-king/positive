module Generated.Data.ImageSettings exposing (FilmRollSettings, ImageCrop, ImageSettings, decodeFilmRollSettings, decodeImageCrop, decodeImageSettings, encodeFilmRollSettings, encodeImageCrop, encodeImageSettings)

import Dict
import Json.Decode
import Json.Decode.Pipeline
import Json.Encode
import Maybe.Extra
import Set


type alias ImageSettings =
    { iFilename : String
    , iRotate : Float
    , iCrop : ImageCrop
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
        , ( "iCrop", encodeImageCrop a.iCrop )
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
        |> Json.Decode.Pipeline.required "iCrop" decodeImageCrop
        |> Json.Decode.Pipeline.required "iGamma" Json.Decode.float
        |> Json.Decode.Pipeline.required "iZone1" Json.Decode.float
        |> Json.Decode.Pipeline.required "iZone5" Json.Decode.float
        |> Json.Decode.Pipeline.required "iZone9" Json.Decode.float
        |> Json.Decode.Pipeline.required "iBlackpoint" Json.Decode.float
        |> Json.Decode.Pipeline.required "iWhitepoint" Json.Decode.float


type alias ImageCrop =
    { icTop : Float, icLeft : Float, icWidth : Float }


encodeImageCrop : ImageCrop -> Json.Encode.Value
encodeImageCrop a =
    Json.Encode.object
        [ ( "icTop", Json.Encode.float a.icTop )
        , ( "icLeft", Json.Encode.float a.icLeft )
        , ( "icWidth", Json.Encode.float a.icWidth )
        ]


decodeImageCrop : Json.Decode.Decoder ImageCrop
decodeImageCrop =
    Json.Decode.succeed ImageCrop
        |> Json.Decode.Pipeline.required "icTop" Json.Decode.float
        |> Json.Decode.Pipeline.required "icLeft" Json.Decode.float
        |> Json.Decode.Pipeline.required "icWidth" Json.Decode.float


type alias FilmRollSettings =
    { frsPoster : Maybe String
    , frsSettings : Dict.Dict String ImageSettings
    , frsStarred : Set.Set String
    }


encodeFilmRollSettings : FilmRollSettings -> Json.Encode.Value
encodeFilmRollSettings a =
    Json.Encode.object
        [ ( "frsPoster", Maybe.Extra.unwrap Json.Encode.null Json.Encode.string a.frsPoster )
        , ( "frsSettings", Json.Encode.dict identity encodeImageSettings a.frsSettings )
        , ( "frsStarred", Json.Encode.set Json.Encode.string a.frsStarred )
        ]


decodeFilmRollSettings : Json.Decode.Decoder FilmRollSettings
decodeFilmRollSettings =
    Json.Decode.succeed FilmRollSettings
        |> Json.Decode.Pipeline.required "frsPoster" (Json.Decode.nullable Json.Decode.string)
        |> Json.Decode.Pipeline.required "frsSettings" (Json.Decode.dict decodeImageSettings)
        |> Json.Decode.Pipeline.required "frsStarred" (Json.Decode.map Set.fromList (Json.Decode.list Json.Decode.string))
