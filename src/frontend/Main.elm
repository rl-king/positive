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
import List.Zipper as Zipper exposing (Zipper)
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
    { imageProcessingState : ImageProcessingState
    , filmRoll : Maybe (Zipper ImageSettings)
    , key : Navigation.Key
    , imageWidth : Maybe Int
    , route : { dir : String, filename : String }
    }


type ImageProcessingState
    = Ready
    | Loading
    | Queued (Maybe (Zipper ImageSettings))


newSettings : String -> ImageSettings
newSettings filename =
    ImageSettings filename 0 0 2.2 0 0 0 0 0


init : () -> Url.Url -> Navigation.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        route =
            fromUrl url
    in
    ( { imageProcessingState = Loading
      , filmRoll = Nothing
      , key = key
      , imageWidth = Nothing
      , route = route
      }
    , Cmd.map GotFilmRollSettings <|
        Request.getImageSettings route.dir
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
                route =
                    fromUrl url
            in
            ( { model
                | filmRoll =
                    Maybe.andThen (Zipper.findFirst (\x -> x.iFilename == route.filename))
                        model.filmRoll
                , imageProcessingState = Loading
                , route = route
              }
            , Cmd.none
            )

        GotFilmRollSettings (Ok settings) ->
            ( { model
                | filmRoll =
                    Maybe.andThen (Zipper.findFirst (\x -> x.iFilename == model.route.filename)) <|
                        Zipper.fromList (List.sortBy .iFilename settings)
              }
            , Task.attempt GotImageDimensions <|
                Browser.Dom.getElement "image-section"
            )

        GotFilmRollSettings (Err err) ->
            ( model, Cmd.none )

        GotSaveImageSettings (Ok settings) ->
            ( { model | filmRoll = Zipper.fromList (List.sortBy .iFilename settings) }
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
                    ( { model | imageProcessingState = Loading, filmRoll = n }, Cmd.none )

        SaveSettings settings ->
            ( model
            , Cmd.map GotSaveImageSettings <|
                Request.postImageSettings model.route.dir settings
            )

        PreviousImage ->
            let
                filename =
                    Maybe.map (\z -> Maybe.withDefault (Zipper.last z) (Zipper.previous z)) model.filmRoll
                        |> Maybe.map (.iFilename << Zipper.current)
                        |> Maybe.withDefault ""
            in
            ( model
            , Navigation.pushUrl model.key <|
                toUrl model.route.dir filename
            )

        NextImage ->
            let
                filename =
                    Maybe.map (\z -> Maybe.withDefault (Zipper.first z) (Zipper.next z)) model.filmRoll
                        |> Maybe.map (.iFilename << Zipper.current)
                        |> Maybe.withDefault ""
            in
            ( model
            , Navigation.pushUrl model.key <|
                toUrl model.route.dir filename
            )


updateSettings : (ImageSettings -> ImageSettings) -> Model -> Model
updateSettings f model =
    let
        mapCurrent =
            Maybe.map (Zipper.mapCurrent f)
    in
    case model.imageProcessingState of
        Ready ->
            { model
                | imageProcessingState = Loading
                , filmRoll = mapCurrent model.filmRoll
            }

        Loading ->
            { model | imageProcessingState = Queued (mapCurrent model.filmRoll) }

        Queued n ->
            { model | imageProcessingState = Queued (mapCurrent n) }



-- VIEW


view : Model -> Browser.Document Msg
view model =
    case model.filmRoll of
        Nothing ->
            { title = "Positive"
            , body =
                [ main_ []
                    [ text "loading" ]
                ]
            }

        Just filmRoll ->
            { title = "Positive"
            , body =
                [ main_ []
                    [ viewImage filmRoll model
                    , viewSettings filmRoll model
                    , viewFileBrowser model.route.dir filmRoll
                    ]
                ]
            }



-- FILE BROWSER


viewFileBrowser : String -> Zipper ImageSettings -> Html Msg
viewFileBrowser dir filmRoll =
    section [ id "files" ]
        [ ul [] <|
            List.concat
                [ List.map (viewFileLink "" dir) (Zipper.before filmRoll)
                , [ viewFileLink "-current" dir (Zipper.current filmRoll) ]
                , List.map (viewFileLink "" dir) (Zipper.after filmRoll)
                ]
        ]


viewFileLink : String -> String -> ImageSettings -> Html Msg
viewFileLink className dir imageSettings =
    li [ class className ]
        [ a
            [ href <|
                toUrl imageSettings.iFilename dir
            ]
            [ text imageSettings.iFilename ]
        ]


toUrl : String -> String -> String
toUrl dir filename =
    Url.Builder.absolute []
        [ Url.Builder.string "filename" filename
        , Url.Builder.string "dir" dir
        ]



-- IMAGE SETTINGS


viewSettings : Zipper ImageSettings -> Model -> Html Msg
viewSettings filmRoll model =
    let
        settings =
            Zipper.current filmRoll
    in
    section [ id "image-settings" ]
        [ div []
            [ label []
                [ text "gamma"
                , text " "
                , text (String.fromFloat settings.iGamma)
                ]
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
        , button [ onClick Rotate ] [ text "Rotate" ]
        , button [ onClick (SaveSettings settings) ] [ text "Save" ]
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


viewImage : Zipper ImageSettings -> Model -> Html Msg
viewImage filmRoll model =
    section [ id "image-section" ]
        [ viewMaybe model.imageWidth <|
            \imageWidth ->
                img
                    [ src <|
                        Url.Builder.absolute [ "image" ]
                            [ toImageUrlParams (Zipper.current filmRoll)
                            , Url.Builder.int "preview-width" imageWidth
                            , Url.Builder.string "dir" model.route.dir
                            ]
                    , on "load" (Decode.succeed OnImageLoad)
                    , style "user-select" "none"
                    ]
                    []

        -- , viewZones model
        ]



-- viewZones : Model -> Html Msg
-- viewZones model =
--     let
--         settings =
--             Zipper.current model.filmRoll
--         zone t i v =
--             v + (i * m t v)
--         m t v =
--             (1 - abs (v - t)) * (1 - abs (v - t))
--         vs =
--             List.map (\x -> toFloat x / 10) <| List.range 0 10
--     in
--     ul [] <|
--         List.map2 (\x i -> li [] [ text (String.left 5 (String.fromFloat (x - i))) ])
--             (List.map (zone 0.95 settings.iZone9 << zone 0.5 settings.iZone5 << zone 0.15 settings.iZone1) vs)
--             vs


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
