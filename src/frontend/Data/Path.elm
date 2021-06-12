module Data.Path exposing
    ( Directory
    , Filename
    , Path
    , compare
    , fromJson
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


toString : Path a -> String
toString (Path a) =
    a


compare : Path a -> Path b -> Order
compare (Path a) (Path b) =
    Basics.compare a b


fromJson : Decode.Decoder (Path a)
fromJson =
    Decode.map Path Decode.string


toJson : Path a -> Encode.Value
toJson (Path a) =
    Encode.string a
