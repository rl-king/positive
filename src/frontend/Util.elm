module Util exposing
    ( HttpResult
    , Route
    , matchKey
    , pushNotification
    , toUrl
    , viewIf
    , viewMaybe
    , withCtrl
    )

import Html exposing (..)
import Http
import Json.Decode as Decode
import Process
import Task
import Url.Builder


type alias Route =
    { dir : String
    , filename : String
    }


type alias HttpResult a =
    Result ( Http.Error, Maybe { metadata : Http.Metadata, body : String } ) a


pushNotification : msg -> String -> { a | notifications : List String } -> ( { a | notifications : List String }, Cmd msg )
pushNotification clearMsg notification model =
    ( { model | notifications = notification :: model.notifications }
    , Task.perform (\_ -> clearMsg) <|
        Process.sleep 5000
    )


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


matchKey : String -> msg -> Decode.Decoder msg
matchKey key msg =
    Decode.field "key" Decode.string
        |> Decode.andThen
            (\s ->
                if key == s then
                    Decode.succeed msg

                else
                    Decode.fail "Not an match"
            )


withCtrl : Decode.Decoder a -> Decode.Decoder a
withCtrl decoder =
    Decode.field "ctrlKey" Decode.bool
        |> Decode.andThen
            (\ctrlPressed ->
                if ctrlPressed then
                    decoder

                else
                    Decode.fail "No ctrl pressed"
            )
