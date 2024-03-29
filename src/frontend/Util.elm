module Util exposing
    ( HttpResult
    , Level(..)
    , NotificationId
    , Notifications
    , Status(..)
    , appendNotifications
    , choice
    , emptyNotifications
    , groupBy
    , matchKey
    , matchKeyNoModifiers
    , mergeStatus
    , pushNotification
    , removeNotification
    , sortByDateAsc
    , sortByDateDesc
    , sortByRollNumberAsc
    , sortByRollNumberDesc
    , viewIf
    , viewMaybe
    , viewNotifications
    , withAlt
    , withCtrl
    )

import Data.Path as Path exposing (Path)
import Date exposing (Date)
import Dict
import Generated.Data exposing (RollNumber(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
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



-- KEYS


matchKey : String -> msg -> Decode.Decoder msg
matchKey key msg =
    decodeKey key
        |> Decode.andThen
            (\k ->
                if key == k.key then
                    Decode.succeed msg

                else
                    Decode.fail "Not an match"
            )


matchKeyNoModifiers : String -> msg -> Decode.Decoder msg
matchKeyNoModifiers key msg =
    decodeKey key
        |> Decode.andThen
            (\k ->
                if key == k.key && not k.ctrl && not k.alt then
                    Decode.succeed msg

                else
                    Decode.fail "Not an match"
            )


decodeKey : String -> Decode.Decoder { alt : Bool, ctrl : Bool, key : String }
decodeKey key =
    Decode.map3
        (\a b c -> { key = a, ctrl = b, alt = c })
        (Decode.field "key" Decode.string)
        (Decode.field "ctrlKey" Decode.bool)
        (Decode.field "altKey" Decode.bool)


withCtrl : Decode.Decoder a -> Decode.Decoder a
withCtrl =
    withKey "ctrlKey"


withAlt : Decode.Decoder a -> Decode.Decoder a
withAlt =
    withKey "altKey"


withKey : String -> Decode.Decoder a -> Decode.Decoder a
withKey key decoder =
    Decode.field key Decode.bool
        |> Decode.andThen
            (\keyPressed ->
                if keyPressed then
                    decoder

                else
                    Decode.fail ("No " ++ key ++ " pressed")
            )


groupBy : (a -> comparable) -> List a -> List ( comparable, List a )
groupBy f xs =
    let
        insert x =
            Dict.update (f x)
                (\ys ->
                    case ys of
                        Nothing ->
                            Just [ x ]

                        Just y ->
                            Just (x :: y)
                )
    in
    Dict.toList <|
        List.foldl insert Dict.empty xs


sortByDateAsc : (a -> Date) -> List a -> List a
sortByDateAsc f =
    List.sortWith (\a b -> Date.compare (f a) (f b))


sortByDateDesc : (a -> Date) -> List a -> List a
sortByDateDesc f =
    List.reverse << sortByDateAsc f


sortByRollNumberAsc :
    List { a | rollNumber : RollNumber }
    -> List { a | rollNumber : RollNumber }
sortByRollNumberAsc =
    let
        compare_ (RollNumber a) (RollNumber b) =
            compare a b
    in
    List.sortWith (\a b -> compare_ a.rollNumber b.rollNumber)


sortByRollNumberDesc :
    List { a | rollNumber : RollNumber }
    -> List { a | rollNumber : RollNumber }
sortByRollNumberDesc =
    List.reverse << sortByRollNumberAsc
