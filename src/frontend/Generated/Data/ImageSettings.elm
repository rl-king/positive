module Generated.Data.ImageSettings exposing (Fs(..), ImageCrop, ImageSettings, WorkingDirectory, decodeFs, decodeImageCrop, decodeImageSettings, decodeWorkingDirectory, encodeFs, encodeImageCrop, encodeImageSettings, encodeWorkingDirectory)

import Json.Decode
import Json.Decode.Pipeline
import Json.Encode


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


type alias WorkingDirectory =
    { unWorkingDirectory : String }


encodeWorkingDirectory : WorkingDirectory -> Json.Encode.Value
encodeWorkingDirectory a =
    Json.Encode.object [ ( "unWorkingDirectory", Json.Encode.string a.unWorkingDirectory ) ]


decodeWorkingDirectory : Json.Decode.Decoder WorkingDirectory
decodeWorkingDirectory =
    Json.Decode.succeed WorkingDirectory
        |> Json.Decode.Pipeline.required "unWorkingDirectory" Json.Decode.string


type Fs
    = File String
    | Dir String (List Fs)


encodeFs : Fs -> Json.Encode.Value
encodeFs a =
    case a of
        File b ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "File" )
                , ( "contents", Json.Encode.string b )
                ]

        Dir b c ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "Dir" )
                , ( "contents"
                  , Json.Encode.list identity
                        [ Json.Encode.string b
                        , Json.Encode.list encodeFs c
                        ]
                  )
                ]


decodeFs : Json.Decode.Decoder Fs
decodeFs =
    Json.Decode.field "tag" Json.Decode.string
        |> Json.Decode.andThen
            (\a ->
                case a of
                    "File" ->
                        Json.Decode.succeed File
                            |> Json.Decode.Pipeline.required "contents" Json.Decode.string

                    "Dir" ->
                        Json.Decode.field "contents"
                            (Json.Decode.succeed Dir
                                |> Json.Decode.Pipeline.custom (Json.Decode.index 0 Json.Decode.string)
                                |> Json.Decode.Pipeline.custom (Json.Decode.index 1 (Json.Decode.list decodeFs))
                            )

                    _ ->
                        Json.Decode.fail "No matching constructor"
            )
