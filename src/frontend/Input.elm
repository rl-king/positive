module Input exposing (viewRange)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode


viewRange : (Float -> msg) -> Float -> ( Float, Float, Float ) -> String -> Float -> Html msg
viewRange toMsg stepSize ( min, max, startingValue ) title val =
    let
        deDupe v =
            if v == val then
                Decode.fail "Is same as val"

            else
                Decode.succeed (toMsg v)
    in
    div
        [ class "range-slider"
        , on "dblclick" <|
            Decode.andThen deDupe (Decode.succeed startingValue)
        ]
        [ label [] [ span [] [ text title ], span [] [ text (String.fromFloat val) ] ]
        , input
            [ type_ "range"
            , value (String.fromFloat val)
            , step (String.fromFloat stepSize)
            , Html.Attributes.min (String.fromFloat min)
            , Html.Attributes.max (String.fromFloat max)
            , on "input" <|
                Decode.andThen deDupe <|
                    Decode.at [ "target", "valueAsNumber" ] Decode.float
            ]
            []
        ]
