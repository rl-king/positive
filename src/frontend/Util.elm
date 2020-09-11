module Util exposing
    ( HttpResult
    , Level(..)
    , Route
    , choice
    , matchKey
    , pushNotification
    , toUrl
    , viewIf
    , viewMaybe
    , viewNotifications
    , withCtrl
    )

import Html exposing (..)
import Html.Attributes exposing (..)
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


type Level
    = Normal
    | Warning
    | Server


pushNotification :
    Level
    -> msg
    -> String
    -> { a | notifications : List ( Level, String ) }
    -> ( { a | notifications : List ( Level, String ) }, Cmd msg )
pushNotification level clearMsg notification model =
    ( { model | notifications = ( level, notification ) :: model.notifications }
    , Task.perform (\_ -> clearMsg) <|
        Process.sleep 3500
    )


viewNotifications : List ( Level, String ) -> Html msg
viewNotifications notifications =
    let
        levelToClass level =
            case level of
                Warning ->
                    "warning"

                Server ->
                    "server"

                Normal ->
                    "normal"
    in
    div [ class "notifications" ] <|
        List.map (\( l, x ) -> span [ class (levelToClass l) ] [ text x ]) (List.reverse notifications)


toUrl : Route -> String
toUrl route =
    Url.Builder.absolute []
        [ Url.Builder.string "filename" route.filename
        , Url.Builder.string "dir" route.dir
        ]


choice : List (Maybe a) -> Maybe a
choice =
    List.head << List.filterMap identity


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
