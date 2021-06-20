module Util.Date exposing (decodeDate, encodeDate)

import Date exposing (Date)
import Json.Decode as Decode
import Json.Encode as Encode


decodeDate : Decode.Decoder Date
decodeDate =
    let
        toDate s =
            case Date.fromIsoString s of
                Ok date ->
                    Decode.succeed date

                Err err ->
                    Decode.fail err
    in
    Decode.andThen toDate Decode.string


encodeDate : Date -> Encode.Value
encodeDate =
    Encode.string << Date.toIsoString
