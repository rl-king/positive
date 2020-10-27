module Input exposing (viewRange, viewRangeInt)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode


viewRangeInt : (Int -> msg) -> Int -> ( Int, Int, Int ) -> String -> Int -> Html msg
viewRangeInt =
    viewRangeInternal String.fromInt Decode.int


viewRange : (Float -> msg) -> Float -> ( Float, Float, Float ) -> String -> Float -> Html msg
viewRange =
    viewRangeInternal String.fromFloat Decode.float


viewRangeInternal :
    (number -> String)
    -> Decode.Decoder number
    -> (number -> msg)
    -> number
    -> ( number, number, number )
    -> String
    -> number
    -> Html msg
viewRangeInternal toString decoder toMsg stepSize ( min, max, startingValue ) title val =
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
        [ label [] [ span [] [ text title ], span [] [ text (toString val) ] ]
        , input
            [ type_ "range"
            , value (toString val)
            , step (toString stepSize)
            , Html.Attributes.min (toString min)
            , Html.Attributes.max (toString max)
            , on "input" <|
                Decode.andThen deDupe <|
                    Decode.at [ "target", "valueAsNumber" ] decoder
            ]
            []
        ]
