module Main exposing (main)

import Base64
import Browser
import Browser.Dom
import Browser.Navigation as Navigation
import Dict exposing (Dict)
import Generated.Data.ImageSettings as ImageSettings exposing (ImageSettings)
import Generated.Request as Request
import Html exposing (..)
import Html.Attributes as Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Task
import Url exposing (Url)
import Url.Builder
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
    , filmRollSettings : Dict String ImageSettings
    , coordinateValue : Maybe Float
    , drag : Maybe Drag
    , key : Navigation.Key
    , fileNames : List String
    , imageWidth : Maybe Int
    }


type Image
    = Ready ImageSettings
    | Loading ImageSettings
    | Queued ImageSettings ImageSettings


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
    ( { image = Ready (ImageSettings (fromUrl url) 2.2 0 0 0 0 0)
      , filmRollSettings = Dict.empty
      , coordinateValue = Nothing
      , drag = Nothing
      , key = key
      , fileNames = []
      , imageWidth = Nothing
      }
    , Cmd.batch
        [ Cmd.map GotDirectory <|
            Request.getImageList "assets"
        , Cmd.map GotFilmRollSettings <|
            Request.getImageSettings "assets"
        , Task.attempt GotImageDimensions <|
            Browser.Dom.getElement "image-section"
        ]
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


type alias HttpResult a =
    Result ( Http.Error, Maybe { metadata : Http.Metadata, body : String } ) a


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | GotDirectory (HttpResult (List String))
    | GotSaveImageSettings (HttpResult ())
    | GotFilmRollSettings (HttpResult (List ImageSettings))
    | GotImageDimensions (Result Browser.Dom.Error Browser.Dom.Element)
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
    | SaveSettings ImageSettings


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked (Browser.Internal url) ->
            ( model, Navigation.pushUrl model.key (Url.toString url) )

        LinkClicked (Browser.External href) ->
            ( model, Navigation.load href )

        UrlChanged url ->
            let
                currentSettings =
                    toSettings model.image
            in
            ( { model | image = Ready { currentSettings | iPath = fromUrl url } }
            , Cmd.none
            )

        GotDirectory (Ok fileNames) ->
            ( { model | fileNames = fileNames }, Cmd.none )

        GotDirectory (Err err) ->
            ( model, Cmd.none )

        GotFilmRollSettings (Ok settings) ->
            ( { model
                | filmRollSettings =
                    Dict.fromList <|
                        List.map (\s -> ( s.iPath, s )) settings
              }
            , Cmd.none
            )

        GotFilmRollSettings (Err err) ->
            ( model, Cmd.none )

        GotSaveImageSettings _ ->
            ( model, Cmd.none )

        GotImageDimensions result ->
            ( { model
                | imageWidth =
                    Result.toMaybe <|
                        Result.map (floor << .width << .element) result
              }
            , Cmd.none
            )

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
                    Url.Builder.absolute [ "image", "coordinate" ]
                        [ toImageUrlParams (toSettings model.image) ]
                , expect = Http.expectJson GotValueAtCoordinate Decode.float
                , body = Http.jsonBody (Encode.list identity [ Encode.int x, Encode.int y ])
                }
            )

        OnGammaChange gamma ->
            ( updateSettings (\s -> { s | iGamma = toFloat gamma / 100 }) model, Cmd.none )

        OnZone1Change zone ->
            ( updateSettings (\s -> { s | iZone1 = toFloat zone / 1000 }) model, Cmd.none )

        OnZone5Change zone ->
            ( updateSettings (\s -> { s | iZone5 = toFloat zone / 1000 }) model, Cmd.none )

        OnZone9Change zone ->
            ( updateSettings (\s -> { s | iZone9 = toFloat zone / 1000 }) model, Cmd.none )

        OnBlackpointChange bp ->
            ( updateSettings (\s -> { s | iBlackpoint = toFloat bp / 100 }) model, Cmd.none )

        OnWhitepointChange wp ->
            ( updateSettings (\s -> { s | iWhitepoint = toFloat wp / 100 }) model, Cmd.none )

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

        SaveSettings settings ->
            ( model
            , Cmd.map GotSaveImageSettings <|
                Request.postImageSettings "assets" settings
            )


updateSettings : (ImageSettings -> ImageSettings) -> Model -> Model
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
            , viewSettings model
            , viewFiles model.fileNames
            ]
        ]
    }


viewFiles : List String -> Html Msg
viewFiles fileNames =
    section [ id "files" ]
        [ ul [] <|
            List.map viewFile (List.sort fileNames)
        ]


viewFile : String -> Html Msg
viewFile fileName =
    li []
        [ a [ href (Url.Builder.absolute [] [ Url.Builder.string "path" ("assets/" ++ fileName) ]) ]
            [ text fileName ]
        ]


viewSettings : Model -> Html Msg
viewSettings model =
    let
        settings =
            toSettings model.image
    in
    section [ id "image-settings" ]
        [ div []
            [ label [] [ text "gamma" ]
            , p [] [ text (String.fromFloat settings.iGamma) ]
            , input
                [ type_ "range"
                , value (String.fromInt (floor (settings.iGamma * 100)))
                , Attributes.min "0"
                , Attributes.max "2000"
                , on "input" <|
                    Decode.map OnGammaChange
                        (Decode.at [ "target", "valueAsNumber" ] Decode.int)
                ]
                []
            ]
        , viewZoneSlider OnZone1Change ( -100, 100 ) "Zone I" (settings.iZone1 * 1000)
        , viewZoneSlider OnZone5Change ( -100, 100 ) "Zone V" (settings.iZone5 * 1000)
        , viewZoneSlider OnZone9Change ( -100, 100 ) "Zone IX" (settings.iZone9 * 1000)
        , viewZoneSlider OnBlackpointChange ( -100, 100 ) "Blackpoint" (settings.iBlackpoint * 100)
        , viewZoneSlider OnWhitepointChange ( -100, 100 ) "Whitepoint" (settings.iWhitepoint * 100)
        , button [ onClick (SaveSettings settings) ] [ text "save" ]
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
            ]
            []
        ]


viewImage : Model -> Html Msg
viewImage model =
    section [ id "image-section" ]
        [ viewMaybe model.imageWidth <|
            \imageWidth ->
                img
                    [ src <|
                        Url.Builder.absolute [ "image" ]
                            [ toImageUrlParams (toSettings model.image)
                            , Url.Builder.int "preview-width" imageWidth
                            ]
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

        -- , viewZones model
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
            (List.map (zone 0.95 settings.iZone9 << zone 0.5 settings.iZone5 << zone 0.15 settings.iZone1) vs)
            vs


toSettings : Image -> ImageSettings
toSettings image =
    case image of
        Ready s ->
            s

        Loading s ->
            s

        Queued s _ ->
            s


toImageUrlParams : ImageSettings -> Url.Builder.QueryParameter
toImageUrlParams =
    Url.Builder.string "image-settings"
        << Base64.encode
        << Encode.encode 0
        << ImageSettings.encodeImageSettings


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



-- HELPERS


viewMaybe : Maybe a -> (a -> Html msg) -> Html msg
viewMaybe maybeA html1 =
    case maybeA of
        Just a ->
            html1 a

        Nothing ->
            text ""
