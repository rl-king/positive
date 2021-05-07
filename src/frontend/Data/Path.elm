module Data.Path exposing
    ( Directory
    , Filename
    , fromJson
    , fromString
    , toJson
    , toString
    )

import Json.Decode as Decode
import Json.Encode as Encode



-- DEFINITIONS


type Path a
    = Path String


type alias Filename =
    Path { filename : () }


type alias Directory =
    Path { directory : () }



-- CONVERSIONS


fromString : String -> Path a
fromString =
    Path


toString : Path a -> String
toString (Path a) =
    a


fromJson : Decode.Decoder (Path a)
fromJson =
    Decode.map Path Decode.string


toJson : Path a -> Encode.Value
toJson (Path a) =
    Encode.string a
