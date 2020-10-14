module Util exposing
    ( EditorRoute
    , HttpResult
    , Level(..)
    , Route(..)
    , choice
    , fromUrl
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
import String.Interpolate exposing (interpolate)
import Task
import Url exposing (Url)
import Url.Builder
import Url.Parser exposing ((</>), (<?>))
import Url.Parser.Query


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



-- URL


type Route
    = Browser { minimumRating : Maybe Int }
    | Editor EditorRoute
    | DecodeError String


type alias EditorRoute =
    { dir : String
    , filename : String
    }


fromUrl : Url -> Route
fromUrl url =
    let
        toEditorRoute a b =
            Maybe.withDefault (DecodeError (interpolate "Failed to decode {0} {1}" [ a, b ])) <|
                Maybe.map2 (\x y -> Editor { dir = x, filename = y })
                    (Url.percentDecode a)
                    (Url.percentDecode b)

        parser =
            Url.Parser.oneOf
                [ Url.Parser.map toEditorRoute <|
                    Url.Parser.s "editor"
                        </> Url.Parser.string
                        </> Url.Parser.string
                , Url.Parser.map (\x -> Browser { minimumRating = x }) <|
                    Url.Parser.top
                        <?> Url.Parser.Query.int "rating"
                ]
    in
    Maybe.withDefault (Browser { minimumRating = Nothing }) <|
        Url.Parser.parse parser url


toUrl : Route -> String
toUrl route =
    case route of
        Browser { minimumRating } ->
            Url.Builder.absolute [] <|
                Maybe.withDefault [] <|
                    Maybe.map (List.singleton << Url.Builder.int "rating") minimumRating

        Editor { dir, filename } ->
            Url.Builder.absolute [ "editor", Url.percentEncode dir, Url.percentEncode filename ] []

        DecodeError _ ->
            Url.Builder.absolute [] []


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
