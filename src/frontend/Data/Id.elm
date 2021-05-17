module Data.Id exposing
    ( CollectionId
    , FilmRollId
    , Id
    , ImageSettingsId
    , fromInt
    , fromJson
    , fromString
    , fromUrl
    , toInt
    , toJson
    , toString
    )

import Json.Decode as Decode
import Json.Encode as Encode
import Url.Parser as Parser



-- DEFINITIONS


type Id a
    = Id Int


type alias FilmRollId =
    Id { filmRollId : () }


type alias ImageSettingsId =
    Id { imageSettingsId : () }


type alias CollectionId =
    Id { collectionId : () }



-- CONVERSIONS


fromInt : Int -> Id a
fromInt =
    Id


toInt : Id a -> Int
toInt (Id id) =
    id


toString : Id a -> String
toString =
    String.fromInt << toInt


fromString : String -> Maybe (Id a)
fromString =
    Maybe.map Id << String.toInt


fromUrl : Parser.Parser (Id b -> a) a
fromUrl =
    Parser.map Id Parser.int


fromJson : Decode.Decoder (Id a)
fromJson =
    Decode.map Id Decode.int


toJson : Id a -> Encode.Value
toJson (Id id) =
    Encode.int id
