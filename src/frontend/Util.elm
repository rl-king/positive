module Util exposing (Route, toUrl, viewIf, viewMaybe)

import Html exposing (..)
import Url.Builder


type alias Route =
    { dir : String
    , filename : String
    }


toUrl : Route -> String
toUrl route =
    Url.Builder.absolute []
        [ Url.Builder.string "filename" route.filename
        , Url.Builder.string "dir" route.dir
        ]


viewIf : Bool -> (() -> Html msg) -> Html msg
viewIf pred html =
    if pred then
        html ()

    else
        text ""


viewMaybe : Maybe a -> (a -> Html msg) -> Html msg
viewMaybe maybe html =
    case maybe of
        Just a ->
            html a

        Nothing ->
            text ""
