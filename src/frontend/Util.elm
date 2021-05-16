module Util exposing
    ( HttpResult
    , Level(..)
    , NotificationId
    , Notifications
    , Status(..)
    , appendNotifications
    , choice
    , emptyNotifications
    , matchKey
    , mergeStatus
    , pushNotification
    , removeNotification
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


type Status a
    = Success a
    | Failure
    | Requested
    | Unknown


mergeStatus : Status a -> Status b -> Maybe ( a, b )
mergeStatus aStatus bStatus =
    case ( aStatus, bStatus ) of
        ( Success a, Success b ) ->
            Just ( a, b )

        _ ->
            Nothing


type alias HttpResult a =
    Result ( Http.Error, Maybe { metadata : Http.Metadata, body : String } ) a


type Level
    = Normal
    | Warning
    | Server


type Notifications
    = Notifications (List ( NotificationId, Level, String )) Int


type NotificationId
    = NotificationId Int


emptyNotifications : Notifications
emptyNotifications =
    Notifications [] 0


removeNotification : NotificationId -> Notifications -> Notifications
removeNotification notificationId (Notifications notifications fresh) =
    Notifications
        (List.filter (\( x, _, _ ) -> x /= notificationId) notifications)
        fresh


appendNotifications : Notifications -> Notifications -> Notifications
appendNotifications (Notifications a x) (Notifications b y) =
    Notifications (a ++ b) (Basics.max x y)


pushNotification :
    Level
    -> (NotificationId -> msg)
    -> String
    -> { a | notifications : Notifications }
    -> ( { a | notifications : Notifications }, Cmd msg )
pushNotification level clearMsg message model =
    let
        (Notifications notifications fresh) =
            model.notifications
    in
    ( { model
        | notifications =
            Notifications
                (notifications ++ [ ( NotificationId fresh, level, message ) ])
                (fresh + 1)
      }
    , Task.perform (\_ -> clearMsg (NotificationId fresh)) <|
        Process.sleep 3500
    )


viewNotifications : Notifications -> Html msg
viewNotifications (Notifications notifications _) =
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
        List.map (\( _, l, x ) -> span [ class (levelToClass l) ] [ text x ])
            notifications


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
