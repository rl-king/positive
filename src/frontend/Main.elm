module Main exposing (main)

import Browser
import Browser.Navigation as Navigation
import Html exposing (..)
import Html.Attributes as Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Url exposing (Url)
import Url.Builder as Url
import Url.Parser
import Url.Parser.Query



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }



-- MODEL


type alias Model =
    { gamma : Float
    , zone : Float
    , coordinateValue : Maybe Float
    , drag : Maybe Drag
    , key : Navigation.Key
    , path : String
    }


type alias Coordinate =
    { x : Int
    , y : Int
    }


type alias Drag =
    { start : Coordinate
    , current : Coordinate
    }


init : () -> Url.Url -> Navigation.Key -> ( Model, Cmd Msg )
init _ url key =
    ( { gamma = 2.2
      , zone = 0
      , coordinateValue = Nothing
      , drag = Nothing
      , key = key
      , path =
            Maybe.withDefault "" <|
                Url.Parser.parse
                    (Url.Parser.map
                        (Maybe.withDefault "")
                        (Url.Parser.query (Url.Parser.Query.string "path"))
                    )
                    url
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | DragStart Coordinate
    | DragAt Coordinate
    | DragEnd
    | OnImageClick ( Int, Int )
    | OnGammaChange Int
    | OnZoneChange Int
    | GotValueAtCoordinate (Result Http.Error Float)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked (Browser.Internal url) ->
            ( model, Navigation.pushUrl model.key (Url.toString url) )

        LinkClicked (Browser.External href) ->
            ( model, Navigation.load href )

        UrlChanged _ ->
            ( model, Cmd.none )

        DragStart coordinates ->
            ( { model | drag = Just (Drag coordinates coordinates) }, Cmd.none )

        DragAt coordinates ->
            case model.drag of
                Just { start } ->
                    ( { model | drag = Just (Drag start coordinates) }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        DragEnd ->
            ( { model | drag = Nothing }, Cmd.none )

        OnImageClick ( x, y ) ->
            ( model
            , Http.post
                { url =
                    Url.absolute [ "image", "coordinate" ]
                        [ Url.string "gamma" (String.fromFloat model.gamma)
                        , Url.string "path" model.path
                        ]
                , expect = Http.expectJson GotValueAtCoordinate Decode.float
                , body = Http.jsonBody (Encode.list identity [ Encode.int x, Encode.int y ])
                }
            )

        OnGammaChange gamma ->
            ( { model | gamma = toFloat gamma / 100 }
            , Cmd.none
            )

        OnZoneChange zone ->
            ( { model | zone = toFloat zone / 1000 }
            , Cmd.none
            )

        GotValueAtCoordinate value ->
            ( { model | coordinateValue = Result.toMaybe value }
            , Cmd.none
            )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Positive"
    , body =
        [ main_ []
            [ viewImage model
            , viewZoneDot model.drag
            , div []
                [ label [] [ text "pos" ]
                , p [] [ text (Debug.toString model.coordinateValue) ]
                ]
            , div []
                [ label [] [ text "gamma" ]
                , p [] [ text (String.fromFloat model.gamma) ]
                , input
                    [ type_ "range"
                    , value (String.fromInt (floor (model.gamma * 100)))
                    , Attributes.min "0"
                    , Attributes.max "2000"
                    , on "change" <|
                        Decode.map OnGammaChange
                            (Decode.at [ "target", "valueAsNumber" ] Decode.int)
                    , style "width" "16rem"
                    ]
                    []
                ]
            , div []
                [ label [] [ text "zone III" ]
                , p [] [ text (String.fromFloat model.zone) ]
                , input
                    [ type_ "range"
                    , value (String.fromInt (floor (model.zone * 1000)))
                    , Attributes.min "-100"
                    , Attributes.max "100"
                    , on "change" <|
                        Decode.map OnZoneChange
                            (Decode.at [ "target", "valueAsNumber" ] Decode.int)
                    , style "width" "16rem"
                    ]
                    []
                ]
            ]
        ]
    }


viewImage : Model -> Html Msg
viewImage model =
    section [ id "image-section" ]
        [ img
            [ src <|
                Url.absolute
                    [ "image" ]
                    [ Url.string "gamma" (String.fromFloat model.gamma)
                    , Url.string "zone" (String.fromFloat model.zone)
                    , Url.string "path" model.path
                    ]
            , on "click" <|
                Decode.map4 (\x y tx ty -> OnImageClick ( x - tx, y - ty ))
                    (Decode.field "x" Decode.int)
                    (Decode.field "y" Decode.int)
                    (Decode.at [ "target", "x" ] Decode.int)
                    (Decode.at [ "target", "y" ] Decode.int)
            , style "user-select" "none"
            ]
            []
        ]


viewZoneDot : Maybe Drag -> Html Msg
viewZoneDot drag =
    span
        (style "background-color" "red"
            :: style "width" "2rem"
            :: style "height" "2rem"
            :: style "display" "block"
            :: style "border-radius" "1rem"
            :: style "position" "absolute"
            :: style "top" "0"
            :: style "left" "0"
            :: translateDot drag
            :: onDrag drag
        )
        []


translateDot : Maybe Drag -> Attribute msg
translateDot drag =
    case drag of
        Nothing ->
            style "transform" "translate(0,0)"

        Just { current } ->
            style "transform" <|
                "translate("
                    ++ String.fromInt (current.x - 16)
                    ++ "px,"
                    ++ String.fromInt (current.y - 16)
                    ++ "px)"


onDrag : Maybe Drag -> List (Attribute Msg)
onDrag drag =
    on "mousedown" (Decode.map DragStart decodeCoordinate)
        :: on "mouseup" (Decode.succeed DragEnd)
        -- :: on "mouseleave" (Decode.succeed DragEnd)
        :: on "touchstart" (Decode.map DragStart decodeCoordinate)
        :: on "touchend" (Decode.succeed DragEnd)
        -- :: on "touchcancel" (Decode.succeed DragEnd)
        :: moveEvent drag


moveEvent : Maybe a -> List (Attribute Msg)
moveEvent drag =
    case drag of
        Just _ ->
            [ preventDefaultOn "mousemove" <|
                Decode.map (\c -> ( DragAt c, True )) decodeCoordinate
            , preventDefaultOn "touchmove" <|
                Decode.map (\c -> ( DragAt c, True )) decodeCoordinate
            ]

        Nothing ->
            []


decodeCoordinate : Decode.Decoder Coordinate
decodeCoordinate =
    let
        decoder =
            Decode.map2 Coordinate
                (Decode.field "pageX" (Decode.map floor Decode.float))
                (Decode.field "pageY" (Decode.map floor Decode.float))
    in
    Decode.oneOf
        [ decoder
        , Decode.at [ "touches", "0" ] decoder
        ]
