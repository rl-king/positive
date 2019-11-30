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
        , subscriptions = subscriptions
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- MODEL


type alias Model =
    { image : Image
    , coordinateValue : Maybe Float
    , drag : Maybe Drag
    , key : Navigation.Key
    , path : String
    , fileNames : List String
    }


type Image
    = Ready Settings
    | Loading Settings
    | Queued Settings Settings


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
    ( { image = Ready (Settings 2.2 0 0 0 0 0)
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


type alias Settings =
    { gamma : Float
    , zone1 : Float
    , zone5 : Float
    , zone9 : Float
    , blackpoint : Float
    , whitepoint : Float
    }


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
    | OnBlackpointChange Int
    | OnWhitepointChange Int
    | GotValueAtCoordinate (Result Http.Error Float)
    | OnImageLoad


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
                        (toImageUrlParams model.path (toSettings model.image))
                , expect = Http.expectJson GotValueAtCoordinate Decode.float
                , body = Http.jsonBody (Encode.list identity [ Encode.int x, Encode.int y ])
                }
            )

        OnGammaChange gamma ->
            ( updateSettings (\s -> { s | gamma = toFloat gamma / 100 }) model, Cmd.none )

        OnZone1Change zone ->
            ( updateSettings (\s -> { s | zone1 = toFloat zone / 1000 }) model, Cmd.none )

        OnZone5Change zone ->
            ( updateSettings (\s -> { s | zone5 = toFloat zone / 1000 }) model, Cmd.none )

        OnZone9Change zone ->
            ( updateSettings (\s -> { s | zone9 = toFloat zone / 1000 }) model, Cmd.none )

        OnBlackpointChange bp ->
            ( updateSettings (\s -> { s | blackpoint = toFloat bp / 100 }) model, Cmd.none )

        OnWhitepointChange wp ->
            ( updateSettings (\s -> { s | whitepoint = toFloat wp / 100 }) model, Cmd.none )

        GotValueAtCoordinate value ->
            ( { model | coordinateValue = Result.toMaybe value }
            , Cmd.none
            )

        OnImageLoad ->
            case model.image of
                Ready _ ->
                    ( model, Cmd.none )

                Loading s ->
                    ( { model | image = Ready s }, Cmd.none )

                Queued _ n ->
                    ( { model | image = Ready n }, Cmd.none )


updateSettings : (Settings -> Settings) -> Model -> Model
updateSettings f model =
    case model.image of
        Ready s ->
            { model | image = Loading (f s) }

        Loading s ->
            { model | image = Queued s (f s) }

        Queued s n ->
            { model | image = Queued s (f n) }



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
        List.map viewFile (List.sort fileNames)


viewFile : String -> Html Msg
viewFile fileName =
    li []
        [ a [ href (Url.absolute [] [ Url.string "path" ("assets/" ++ fileName) ]) ]
            [ text fileName ]
        ]


viewSettings : Model -> Html Msg
viewSettings model =
    let
        settings =
            toSettings model.image
    in
    section [ id "settings" ]
        [ div []
            [ label [] [ text "pos" ]
            , p [] [ text (Debug.toString model.coordinateValue) ]
            ]
        , div []
            [ label [] [ text "gamma" ]
            , p [] [ text (String.fromFloat settings.gamma) ]
            , input
                [ type_ "range"
                , value (String.fromInt (floor (settings.gamma * 100)))
                , Attributes.min "0"
                , Attributes.max "2000"
                , on "input" <|
                    Decode.map OnGammaChange
                        (Decode.at [ "target", "valueAsNumber" ] Decode.int)
                , style "width" "16rem"
                ]
                []
            ]
        , viewZoneSlider OnZone1Change ( -100, 100 ) "Zone I" (settings.zone1 * 1000)
        , viewZoneSlider OnZone5Change ( -100, 100 ) "Zone V" (settings.zone5 * 1000)
        , viewZoneSlider OnZone9Change ( -100, 100 ) "Zone IX" (settings.zone9 * 1000)
        , viewZoneSlider OnBlackpointChange ( -100, 100 ) "Blackpoint" (settings.blackpoint * 100)
        , viewZoneSlider OnWhitepointChange ( -100, 100 ) "Whitepoint" (settings.whitepoint * 100)
        ]


viewZoneSlider : (Int -> Msg) -> ( Int, Int ) -> String -> Float -> Html Msg
viewZoneSlider toMsg ( min, max ) title val =
    div []
        [ label [] [ text title ]
        , p [] [ text (String.fromFloat val) ]
        , input
            [ type_ "range"
            , value (String.fromInt (floor val))
            , Attributes.min (String.fromInt min)
            , Attributes.max (String.fromInt max)
            , on "input" <|
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
            [ src (Url.absolute [ "image" ] (toImageUrlParams model.path (toSettings model.image)))
            , on "load" (Decode.succeed OnImageLoad)
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
        settings =
            toSettings model.image

        zone t i v =
            v + (i * m t v)

        m t v =
            (1 - abs (v - t)) * (1 - abs (v - t))

        vs =
            List.map (\x -> toFloat x / 10) <| List.range 0 10
    in
    ul [] <|
        List.map2 (\x i -> li [] [ text (String.left 5 (String.fromFloat (x - i))) ])
            (List.map (zone 0.95 settings.zone9 << zone 0.5 settings.zone5 << zone 0.15 settings.zone1) vs)
            vs


toSettings : Image -> Settings
toSettings image =
    case image of
        Ready s ->
            s

        Loading s ->
            s

        Queued s _ ->
            s


toImageUrlParams : String -> Settings -> List Url.QueryParameter
toImageUrlParams path settings =
    [ Url.string "gamma" (String.fromFloat settings.gamma)
    , Url.string "zone-1" (String.fromFloat settings.zone1)
    , Url.string "zone-5" (String.fromFloat settings.zone5)
    , Url.string "zone-9" (String.fromFloat settings.zone9)
    , Url.string "blackpoint" (String.fromFloat settings.blackpoint)
    , Url.string "whitepoint" (String.fromFloat settings.whitepoint)
    , Url.string "path" path
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
