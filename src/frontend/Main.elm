module Main exposing (main)

import Base64
import Browser
import Browser.Dom
import Browser.Events
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
import List.Zipper as Zipper
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
    Sub.batch
        [ Browser.Events.onKeyDown <|
            matchKey "ArrowLeft" PreviousImage
        , Browser.Events.onKeyDown <|
            matchKey "ArrowRight" NextImage
        ]


matchKey : String -> msg -> Decode.Decoder msg
matchKey key_ msg =
    Decode.field "key" Decode.string
        |> Decode.andThen
            (\s ->
                if key_ == s then
                    Decode.succeed msg

                else
                    Decode.fail "Not an match"
            )



-- MODEL


type alias Model =
    { imageSettings : ImageSettings
    , imageProcessingState : ImageProcessingState
    , filmRollSettings : Dict String ImageSettings
    , key : Navigation.Key
    , fileNames : List String
    , imageWidth : Maybe Int
    , dir : String
    }


type ImageProcessingState
    = Ready
    | Loading
    | Queued ImageSettings


newSettings : String -> ImageSettings
newSettings filename =
    ImageSettings filename 0 0 2.2 0 0 0 0 0


init : () -> Url.Url -> Navigation.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        { dir, filename } =
            fromUrl url
    in
    ( { imageSettings = ImageSettings filename 0 0 2.2 0 0 0 0 0
      , imageProcessingState = Loading
      , filmRollSettings = Dict.empty
      , key = key
      , fileNames = []
      , imageWidth = Nothing
      , dir = dir
      }
    , Cmd.batch
        [ Cmd.map GotDirectory <|
            Request.getImageList dir
        , Cmd.map GotFilmRollSettings <|
            Request.getImageSettings dir
        , Task.attempt GotImageDimensions <|
            Browser.Dom.getElement "image-section"
        ]
    )


fromUrl : Url -> { dir : String, filename : String }
fromUrl url =
    Maybe.withDefault { dir = "", filename = "" } <|
        Url.Parser.parse
            (Url.Parser.query
                (Url.Parser.Query.map2 (\a b -> { dir = Maybe.withDefault "" a, filename = Maybe.withDefault "" b })
                    (Url.Parser.Query.string "dir")
                    (Url.Parser.Query.string "filename")
                )
            )
            url



-- UPDATE


type alias HttpResult a =
    Result ( Http.Error, Maybe { metadata : Http.Metadata, body : String } ) a


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | GotDirectory (HttpResult (List String))
    | GotSaveImageSettings (HttpResult (List ImageSettings))
    | GotFilmRollSettings (HttpResult (List ImageSettings))
    | GotImageDimensions (Result Browser.Dom.Error Browser.Dom.Element)
    | Rotate
    | OnGammaChange Int
    | OnZone1Change Int
    | OnZone5Change Int
    | OnZone9Change Int
    | OnBlackpointChange Int
    | OnWhitepointChange Int
    | OnImageLoad
    | SaveSettings ImageSettings
    | PreviousImage
    | NextImage


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked (Browser.Internal url) ->
            ( model, Navigation.pushUrl model.key (Url.toString url) )

        LinkClicked (Browser.External href) ->
            ( model, Navigation.load href )

        UrlChanged url ->
            let
                settings =
                    Maybe.withDefault (newSettings filename) <|
                        Dict.get filename model.filmRollSettings

                { dir, filename } =
                    fromUrl url
            in
            ( { model
                | imageSettings = settings
                , imageProcessingState = Loading
                , dir = dir
              }
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
                        List.map (\s -> ( s.iFilename, s )) settings
              }
            , Cmd.none
            )

        GotFilmRollSettings (Err err) ->
            ( model, Cmd.none )

        GotSaveImageSettings (Ok settings) ->
            ( { model
                | filmRollSettings =
                    Dict.fromList <|
                        List.map (\s -> ( s.iFilename, s )) settings
              }
            , Cmd.none
            )

        GotSaveImageSettings (Err _) ->
            ( model, Cmd.none )

        GotImageDimensions result ->
            ( { model
                | imageWidth =
                    Result.toMaybe <|
                        Result.map (floor << .width << .element) result
              }
            , Cmd.none
            )

        Rotate ->
            ( updateSettings (\s -> { s | iRotate = s.iRotate + degrees -90 }) model, Cmd.none )

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

        OnImageLoad ->
            case model.imageProcessingState of
                Ready ->
                    ( { model | imageProcessingState = Ready }, Cmd.none )

                Loading ->
                    ( { model | imageProcessingState = Ready }, Cmd.none )

                Queued n ->
                    ( { model | imageProcessingState = Loading, imageSettings = n }, Cmd.none )

        SaveSettings settings ->
            ( model
            , Cmd.map GotSaveImageSettings <|
                Request.postImageSettings model.dir settings
            )

        PreviousImage ->
            ( model, Cmd.none )

        NextImage ->
            ( model, Cmd.none )


updateSettings : (ImageSettings -> ImageSettings) -> Model -> Model
updateSettings f model =
    case model.imageProcessingState of
        Ready ->
            { model | imageProcessingState = Loading, imageSettings = f model.imageSettings }

        Loading ->
            { model | imageProcessingState = Queued (f model.imageSettings) }

        Queued n ->
            { model | imageProcessingState = Queued (f n) }



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Positive"
    , body =
        [ main_ []
            [ viewImage model
            , viewSettings model
            , viewFiles model.dir model.fileNames
            ]
        ]
    }


viewFiles : String -> List String -> Html Msg
viewFiles dir fileNames =
    section [ id "files" ]
        [ ul [] <|
            List.map (viewFile dir) (List.sort fileNames)
        ]


viewFile : String -> String -> Html Msg
viewFile dir fileName =
    li []
        [ a
            [ href <|
                Url.Builder.absolute []
                    [ Url.Builder.string "filename" fileName
                    , Url.Builder.string "dir" dir
                    ]
            ]
            [ text fileName ]
        ]


viewSettings : Model -> Html Msg
viewSettings model =
    section [ id "image-settings" ]
        [ div []
            [ label []
                [ text "gamma"
                , text " "
                , text (String.fromFloat model.imageSettings.iGamma)
                ]
            , input
                [ type_ "range"
                , value (String.fromInt (floor (model.imageSettings.iGamma * 100)))
                , Attributes.min "0"
                , Attributes.max "2000"
                , on "input" <|
                    Decode.map OnGammaChange
                        (Decode.at [ "target", "valueAsNumber" ] Decode.int)
                ]
                []
            ]
        , viewZoneSlider OnZone1Change ( -100, 100 ) "Zone I" (model.imageSettings.iZone1 * 1000)
        , viewZoneSlider OnZone5Change ( -100, 100 ) "Zone V" (model.imageSettings.iZone5 * 1000)
        , viewZoneSlider OnZone9Change ( -100, 100 ) "Zone IX" (model.imageSettings.iZone9 * 1000)
        , viewZoneSlider OnBlackpointChange ( -100, 100 ) "Blackpoint" (model.imageSettings.iBlackpoint * 100)
        , viewZoneSlider OnWhitepointChange ( -100, 100 ) "Whitepoint" (model.imageSettings.iWhitepoint * 100)
        , button [ onClick Rotate ] [ text "Rotate" ]
        , button [ onClick (SaveSettings model.imageSettings) ] [ text "Save" ]
        ]


viewZoneSlider : (Int -> Msg) -> ( Int, Int ) -> String -> Float -> Html Msg
viewZoneSlider toMsg ( min, max ) title val =
    div []
        [ label []
            [ text title
            , text " "
            , text (String.fromFloat val)
            ]
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
                            [ toImageUrlParams model.imageSettings
                            , Url.Builder.int "preview-width" imageWidth
                            , Url.Builder.string "dir" model.dir
                            ]
                    , on "load" (Decode.succeed OnImageLoad)
                    , style "user-select" "none"
                    ]
                    []

        -- , viewZones model
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
            (List.map (zone 0.95 model.imageSettings.iZone9 << zone 0.5 model.imageSettings.iZone5 << zone 0.15 model.imageSettings.iZone1) vs)
            vs


toImageUrlParams : ImageSettings -> Url.Builder.QueryParameter
toImageUrlParams =
    Url.Builder.string "image-settings"
        << Base64.encode
        << Encode.encode 0
        << ImageSettings.encodeImageSettings



-- HELPERS


viewMaybe : Maybe a -> (a -> Html msg) -> Html msg
viewMaybe maybeA html1 =
    case maybeA of
        Just a ->
            html1 a

        Nothing ->
            text ""
