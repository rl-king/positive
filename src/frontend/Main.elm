module Main exposing (main)

import Base64
import Browser
import Browser.Dom
import Browser.Events
import Browser.Navigation as Navigation
import Dict exposing (Dict)
import Generated.Data.ImageSettings as ImageSettings
    exposing
        ( ImageCrop
        , ImageSettings
        )
import Generated.Request as Request
import Html exposing (..)
import Html.Attributes as Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import List.Zipper as Zipper exposing (Zipper)
import String.Interpolate exposing (interpolate)
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
        , Browser.Events.onKeyDown <|
            matchKey "r" Rotate
        ]


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



-- MODEL


type alias Model =
    { imageProcessingState : ImageProcessingState
    , filmRoll : Maybe (Zipper ImageSettings)
    , key : Navigation.Key
    , imageWidth : Maybe ( Int, Scale )
    , imageCropMode : Maybe ImageCrop
    , route : { dir : String, filename : String }
    }


type ImageProcessingState
    = Ready
    | Loading
    | Queued (Maybe (Zipper ImageSettings))


type Scale
    = Half
    | Contain


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
      , imageCropMode = Nothing
      , route = route
      }
    , Cmd.map GotFilmRollSettings <|
        Request.getImageSettings route.dir
    )


fromUrl : Url -> { dir : String, filename : String }
fromUrl url =
    -- FIXME: Don't default
    Maybe.withDefault { dir = "", filename = "" } <|
        Maybe.andThen identity <|
            Url.Parser.parse
                (Url.Parser.query
                    (Url.Parser.Query.map2 (Maybe.map2 (\a b -> { dir = a, filename = b }))
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
    | OnGammaChange Float
    | OnZone1Change Float
    | OnZone5Change Float
    | OnZone9Change Float
    | OnBlackpointChange Float
    | OnWhitepointChange Float
    | OnImageLoad
    | SaveSettings ImageSettings
    | CycleScale Scale
    | Reset
    | UpdateImageCropMode (Maybe ImageCrop)
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

        GotSaveImageSettings _ ->
            ( model, Cmd.none )

        GotImageDimensions result ->
            ( { model
                | imageWidth =
                    Result.toMaybe <|
                        Result.map (\v -> ( floor v.element.width, Contain )) result
              }
            , Cmd.none
            )

        Rotate ->
            ( updateSettings (\s -> { s | iRotate = s.iRotate + degrees -90 }) model, Cmd.none )

        OnGammaChange gamma ->
            ( updateSettings (\s -> { s | iGamma = gamma }) model, Cmd.none )

        OnZone1Change zone ->
            ( updateSettings (\s -> { s | iZone1 = zone }) model, Cmd.none )

        OnZone5Change zone ->
            ( updateSettings (\s -> { s | iZone5 = zone }) model, Cmd.none )

        OnZone9Change zone ->
            ( updateSettings (\s -> { s | iZone9 = zone }) model, Cmd.none )

        OnBlackpointChange bp ->
            ( updateSettings (\s -> { s | iBlackpoint = bp }) model, Cmd.none )

        OnWhitepointChange wp ->
            ( updateSettings (\s -> { s | iWhitepoint = wp }) model, Cmd.none )

        OnImageLoad ->
            case model.imageProcessingState of
                Ready ->
                    ( { model | imageProcessingState = Ready }, Cmd.none )

                Loading ->
                    ( { model | imageProcessingState = Ready }, Cmd.none )

                Queued n ->
                    if n == model.filmRoll then
                        ( { model | imageProcessingState = Ready }, Cmd.none )

                    else
                        ( { model | imageProcessingState = Loading, filmRoll = n }, Cmd.none )

        SaveSettings settings ->
            ( model
            , Cmd.map GotSaveImageSettings <|
                Request.postImageSettings model.route.dir settings
            )

        CycleScale scale ->
            let
                newScale =
                    case scale of
                        Contain ->
                            Half

                        Half ->
                            Contain
            in
            ( { model
                | imageWidth =
                    Maybe.map (Tuple.mapSecond (always newScale))
                        model.imageWidth
                , imageProcessingState = Loading
              }
            , Cmd.none
            )

        Reset ->
            let
                reSettings current =
                    ImageSettings current.iFilename 0 (ImageCrop 0 0 100) 2.2 0 0 0 0 0
            in
            ( { model
                | filmRoll =
                    Maybe.map
                        (Zipper.mapCurrent reSettings)
                        model.filmRoll
              }
            , Cmd.none
            )

        UpdateImageCropMode mode ->
            ( { model | imageCropMode = mode }, Cmd.none )

        PreviousImage ->
            let
                filename =
                    Maybe.map (\z -> Maybe.withDefault (Zipper.last z) (Zipper.previous z)) model.filmRoll
                        |> Maybe.map (.iFilename << Zipper.current)
                        -- FIXME: Don't default
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
                        -- FIXME: Don't default
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
            , body = [ main_ [] [ div [ class "loading-spinner" ] [] ] ]
            }

        Just filmRoll ->
            { title = model.route.filename ++ " | Positive"
            , body =
                [ main_ []
                    [ viewLoading model.imageProcessingState
                    , viewImage filmRoll model
                    , viewSettings filmRoll model
                    , viewFileBrowser model.route.dir filmRoll
                    ]
                ]
            }



-- LOADING


viewLoading : ImageProcessingState -> Html msg
viewLoading state =
    viewIf (state /= Ready) <|
        \_ ->
            div [ class "loading-spinner" ] []



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
                toUrl dir imageSettings.iFilename
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
            [ viewRangeInput OnGammaChange ( 0, 10 ) "Gamma" settings.iGamma
            , viewRangeInput OnZone1Change ( -1, 1 ) "Zone I" settings.iZone1
            , viewRangeInput OnZone5Change ( -1, 1 ) "Zone V" settings.iZone5
            , viewRangeInput OnZone9Change ( -1, 1 ) "Zone IX" settings.iZone9
            , viewRangeInput OnBlackpointChange ( -10, 10 ) "Blackpoint" settings.iBlackpoint
            , viewRangeInput OnWhitepointChange ( -10, 10 ) "Whitepoint" settings.iWhitepoint
            , viewImageCropMode settings model.imageCropMode
            , button [ onClick Rotate ] [ text "Rotate" ]
            , viewMaybe model.imageWidth <|
                \( _, scale ) ->
                    button [ onClick (CycleScale scale) ]
                        [ text "Scale "
                        , text (scaleToString scale)
                        ]
            , button [ onClick (SaveSettings settings) ] [ text "Save" ]
            , button [ onClick Reset ] [ text "Reset" ]
            ]
        ]


viewImageCropMode : ImageSettings -> Maybe ImageCrop -> Html Msg
viewImageCropMode current imageCropMode =
    let
        onTopChange imageCrop v =
            UpdateImageCropMode (Just { imageCrop | icTop = floor v })

        onLeftChange imageCrop v =
            UpdateImageCropMode (Just { imageCrop | icLeft = floor v })

        onWidthChange imageCrop v =
            UpdateImageCropMode (Just { imageCrop | icWidth = v })
    in
    div [] <|
        case imageCropMode of
            Nothing ->
                [ button
                    [ onClick (UpdateImageCropMode (Just current.iCrop))
                    ]
                    [ text "Crop" ]
                ]

            Just imageCrop ->
                [ viewRangeInput (onTopChange imageCrop) ( 0, 100 ) "Top" (toFloat imageCrop.icTop)
                , viewRangeInput (onLeftChange imageCrop) ( 0, 100 ) "Left" (toFloat imageCrop.icLeft)
                , viewRangeInput (onWidthChange imageCrop) ( 0, 100 ) "Width" imageCrop.icWidth
                , button [ onClick (UpdateImageCropMode Nothing) ]
                    [ text "Cancel" ]
                ]


scaleToString : Scale -> String
scaleToString scale =
    case scale of
        Half ->
            "Half"

        Contain ->
            "Contain"


viewRangeInput : (Float -> Msg) -> ( Int, Int ) -> String -> Float -> Html Msg
viewRangeInput toMsg ( min, max ) title val =
    div []
        [ label [] [ text title, text " ", text (String.fromFloat val) ]
        , input
            [ type_ "range"
            , value (String.fromFloat val)
            , step "0.1"
            , Attributes.min (String.fromInt min)
            , Attributes.max (String.fromInt max)
            , on "input" <|
                (Decode.at [ "target", "valueAsNumber" ] Decode.float
                    |> Decode.andThen
                        (\n ->
                            if n == val then
                                Decode.fail "Is same as val"

                            else
                                Decode.succeed (toMsg n)
                        )
                )
            ]
            []
        ]


viewImage : Zipper ImageSettings -> Model -> Html Msg
viewImage filmRoll model =
    let
        applyScale ( width, scale ) =
            case scale of
                Half ->
                    floor (toFloat width * 0.5)

                Contain ->
                    width
    in
    section [ id "image-section" ]
        [ div [ class "image-wrapper" ]
            [ viewMaybe (Maybe.map applyScale model.imageWidth) <|
                \imageWidth ->
                    img
                        [ on "load" (Decode.succeed OnImageLoad)
                        , style "user-select" "none"
                        , src <|
                            Url.Builder.absolute [ "image" ]
                                [ toImageUrlParams (Zipper.current filmRoll)
                                , Url.Builder.int "preview-width" imageWidth
                                , Url.Builder.string "dir" model.route.dir
                                ]
                        ]
                        []
            , viewMaybe model.imageCropMode <|
                \imageCrop ->
                    div
                        [ class "image-crop-overlay"
                        , style "transform" <|
                            interpolate "translate({0}px, {1}px)"
                                [ String.fromInt imageCrop.icLeft
                                , String.fromInt imageCrop.icTop
                                ]
                        , style "width" <|
                            interpolate "{0}%" [ String.fromFloat imageCrop.icWidth ]
                        ]
                        []
            ]
        , viewZones filmRoll model
        ]



-- ZONES


viewZones : Zipper ImageSettings -> Model -> Html Msg
viewZones filmRoll model =
    let
        settings =
            Zipper.current filmRoll

        zone t i v =
            v + (i * m t v)

        m t v =
            (1 - abs (v - t)) * (1 - abs (v - t))

        vs =
            List.map (\x -> toFloat x / 10) <| List.range 1 10

        apply =
            zone 0.95 settings.iZone9 << zone 0.5 settings.iZone5 << zone 0.15 settings.iZone1
    in
    Html.Keyed.ul [ class "zones" ] <|
        List.map2 viewZoneBar (List.map apply vs) vs


viewZoneBar : Float -> Float -> ( String, Html msg )
viewZoneBar value zone =
    let
        height v =
            style "height" (String.fromFloat (1 + 2 * v) ++ "rem")

        background v =
            style "background-color" ("hsl(0, 0%," ++ String.fromFloat (v * 100) ++ "%)")
    in
    ( String.fromFloat value
    , li [ height (value - zone), background zone ]
        []
    )



-- HELPERS


toImageUrlParams : ImageSettings -> Url.Builder.QueryParameter
toImageUrlParams =
    Url.Builder.string "image-settings"
        << Base64.encode
        << Encode.encode 0
        << ImageSettings.encodeImageSettings


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
