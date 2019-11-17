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
    , zone1 : Float
    , zone5 : Float
    , zone9 : Float
    , coordinateValue : Maybe Float
    , drag : Maybe Drag
    , key : Navigation.Key
    , path : String
    , fileNames : List String
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
      , zone1 = 0
      , zone5 = 0
      , zone9 = 0
      , coordinateValue = Nothing
      , drag = Nothing
      , key = key
      , fileNames = []
      , path = fromUrl url
      }
    , Http.get
        { url = Url.absolute [ "directory" ] [ Url.string "dir" "assets" ]
        , expect =
            Http.expectJson GotDirectory (Decode.list Decode.string)
        }
    )


fromUrl : Url -> String
fromUrl url =
    Maybe.withDefault "" <|
        Url.Parser.parse
            (Url.Parser.map
                (Maybe.withDefault "")
                (Url.Parser.query (Url.Parser.Query.string "path"))
            )
            url



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | GotDirectory (Result Http.Error (List String))
    | DragStart Coordinate
    | DragAt Coordinate
    | DragEnd
    | OnImageClick ( Int, Int )
    | OnGammaChange Int
    | OnZone1Change Int
    | OnZone5Change Int
    | OnZone9Change Int
    | GotValueAtCoordinate (Result Http.Error Float)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked (Browser.Internal url) ->
            ( model, Navigation.pushUrl model.key (Url.toString url) )

        LinkClicked (Browser.External href) ->
            ( model, Navigation.load href )

        UrlChanged url ->
            ( { model | path = fromUrl url }, Cmd.none )

        GotDirectory (Ok fileNames) ->
            ( { model | fileNames = fileNames }, Cmd.none )

        GotDirectory (Err err) ->
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
                        (toImageUrlParams model)
                , expect = Http.expectJson GotValueAtCoordinate Decode.float
                , body = Http.jsonBody (Encode.list identity [ Encode.int x, Encode.int y ])
                }
            )

        OnGammaChange gamma ->
            ( { model | gamma = toFloat gamma / 100 }
            , Cmd.none
            )

        OnZone1Change zone ->
            ( { model | zone1 = toFloat zone / 1000 }, Cmd.none )

        OnZone5Change zone ->
            ( { model | zone5 = toFloat zone / 1000 }, Cmd.none )

        OnZone9Change zone ->
            ( { model | zone9 = toFloat zone / 1000 }, Cmd.none )

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
            , viewSettings model
            , viewFiles model.fileNames
            ]
        ]
    }


viewFiles : List String -> Html Msg
viewFiles fileNames =
    ul [] <|
        List.map viewFile fileNames


viewFile : String -> Html Msg
viewFile fileName =
    li []
        [ a [ href (Url.absolute [] [ Url.string "path" ("assets/" ++ fileName) ]) ]
            [ text fileName ]
        ]


viewSettings : Model -> Html Msg
viewSettings model =
    section [ id "settings" ]
        [ div []
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
        , viewZoneSlider OnZone1Change "Zone I" model.zone1
        , viewZoneSlider OnZone5Change "Zone V" model.zone5
        , viewZoneSlider OnZone9Change "Zone IX" model.zone9
        ]


viewZoneSlider : (Int -> Msg) -> String -> Float -> Html Msg
viewZoneSlider toMsg title val =
    div []
        [ label [] [ text title ]
        , p [] [ text (String.fromFloat val) ]
        , input
            [ type_ "range"
            , value (String.fromInt (floor (val * 1000)))
            , Attributes.min "-100"
            , Attributes.max "100"
            , on "change" <|
                Decode.map toMsg
                    (Decode.at [ "target", "valueAsNumber" ] Decode.int)
            , style "width" "16rem"
            ]
            []
        ]


viewImage : Model -> Html Msg
viewImage model =
    section [ id "image-section" ]
        [ img
            [ src (Url.absolute [ "image" ] (toImageUrlParams model))
            , on "click" <|
                Decode.map4 (\x y tx ty -> OnImageClick ( x - tx, y - ty ))
                    (Decode.field "x" Decode.int)
                    (Decode.field "y" Decode.int)
                    (Decode.at [ "target", "x" ] Decode.int)
                    (Decode.at [ "target", "y" ] Decode.int)
            , style "user-select" "none"
            ]
            []
        , viewZones model
        ]


viewZones : Model -> Html Msg
viewZones model =
    let
        zone t i v =
            v + (i * m t v)

        m t v =
            (1 - abs (v - t)) * (1 - abs (v - t))

        vs =
            List.map (\x -> toFloat x / 10) <| List.range 0 10
    in
    ul [] <|
        List.map2 (\x i -> li [] [ text (String.left 5 (String.fromFloat (x - i))) ])
            (List.map (zone 0.95 model.zone9 << zone 0.5 model.zone5 << zone 0.15 model.zone1) vs)
            vs


toImageUrlParams : Model -> List Url.QueryParameter
toImageUrlParams model =
    [ Url.string "gamma" (String.fromFloat model.gamma)
    , Url.string "zone-1" (String.fromFloat model.zone1)
    , Url.string "zone-5" (String.fromFloat model.zone5)
    , Url.string "zone-9" (String.fromFloat model.zone9)
    , Url.string "path" model.path
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
