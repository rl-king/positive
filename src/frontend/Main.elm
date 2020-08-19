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
        , WorkingDirectory
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
import ScrollTo
import String.Interpolate exposing (interpolate)
import Task
import Time
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
        [ Sub.map ScrollToMsg <|
            ScrollTo.subscriptions model.scrollTo
        , Browser.Events.onKeyDown <|
            matchKey "ArrowLeft" PreviousImage
        , Browser.Events.onKeyDown <|
            matchKey "ArrowRight" NextImage
        , Browser.Events.onKeyDown <|
            matchKey "r" Rotate
        , Maybe.withDefault Sub.none <|
            Maybe.map
                (\filmRoll ->
                    Browser.Events.onKeyDown <|
                        Decode.oneOf
                            [ matchKey "s" (SaveSettings filmRoll)
                            , matchKey "c" (CopySettings (Zipper.current filmRoll))
                            ]
                )
                model.filmRoll
        , Maybe.withDefault Sub.none <|
            Maybe.map2
                (\clipboard current ->
                    Browser.Events.onKeyDown <|
                        withCtrl <|
                            Decode.map OnImageSettingsChange <|
                                Decode.oneOf
                                    [ matchKey "a" { clipboard | iFilename = current.iFilename }
                                    , matchKey "c" { current | iCrop = clipboard.iCrop }
                                    , matchKey "t"
                                        { clipboard
                                            | iFilename = current.iFilename
                                            , iRotate = current.iRotate
                                            , iCrop = current.iCrop
                                        }
                                    ]
                )
                model.clipboard
                (Maybe.map Zipper.current model.filmRoll)
        , Maybe.withDefault Sub.none <|
            Maybe.map
                (always <|
                    Browser.Events.onKeyDown <|
                        matchKey "Escape" (UpdateImageCropMode Nothing)
                )
                model.imageCropMode
        , saveSubscription model.saveStatus
        ]


saveSubscription : SaveStatus -> Sub Msg
saveSubscription saveStatus =
    case saveStatus of
        SaveAfter _ _ ->
            Time.every 1000 AttemptSave

        _ ->
            Sub.none


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



-- MODEL


type alias Model =
    { imageProcessingState : ImageProcessingState
    , filmRoll : Maybe FilmRoll
    , key : Navigation.Key
    , imageWidth : Maybe Int
    , imageCropMode : Maybe ImageCrop
    , clipboard : Maybe ImageSettings
    , route : { filename : String }
    , workingDirectory : Maybe WorkingDirectory
    , saveStatus : SaveStatus
    , scrollTo : ScrollTo.State
    , histogram : List Int
    , undoState : List FilmRoll
    }


type alias FilmRoll =
    Zipper ImageSettings


type ImageProcessingState
    = Ready
    | Loading
    | Queued (Maybe FilmRoll)


type SaveStatus
    = Idle
    | SavedAt Time.Posix
    | SaveAfter Int FilmRoll
    | Error Http.Error


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
      , clipboard = Nothing
      , route = route
      , saveStatus = Idle
      , workingDirectory = Nothing
      , scrollTo = ScrollTo.init
      , histogram = []
      , undoState = []
      }
    , Cmd.map GotFilmRollSettings Request.getImageSettings
    )


fromUrl : Url -> { filename : String }
fromUrl url =
    -- FIXME: Don't default
    Maybe.withDefault { filename = "" } <|
        Maybe.andThen identity <|
            Url.Parser.parse
                (Url.Parser.query
                    (Url.Parser.Query.map (Maybe.map (\a -> { filename = a }))
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
    | ScrollToMsg ScrollTo.Msg
    | AttemptSave Time.Posix
    | GotSaveImageSettings (HttpResult (List ImageSettings))
    | GotFilmRollSettings (HttpResult ( List ImageSettings, WorkingDirectory ))
    | GotGenerateHighres (HttpResult ())
    | GotGeneratePreviews (HttpResult ())
    | GotHistogram (HttpResult (List Int))
    | GotImageDimensions (Result Browser.Dom.Error Browser.Dom.Element)
    | Rotate
    | OnImageSettingsChange ImageSettings
    | OnImageLoad ImageSettings Int
    | SaveSettings FilmRoll
    | GenerateHighres ImageSettings
    | GeneratePreviews
    | CopySettings ImageSettings
    | UpdateImageCropMode (Maybe ImageCrop)
    | ApplyCrop ImageCrop
    | PreviousImage
    | NextImage
    | Undo


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
            , Cmd.map ScrollToMsg ScrollTo.scrollToTop
            )

        ScrollToMsg scrollToMsg ->
            let
                ( scrollToModel, scrollToCmds ) =
                    ScrollTo.update scrollToMsg model.scrollTo
            in
            ( { model | scrollTo = scrollToModel }
            , Cmd.map ScrollToMsg scrollToCmds
            )

        AttemptSave currentTime ->
            case model.saveStatus of
                SaveAfter 0 filmRoll ->
                    ( model
                    , Cmd.map GotSaveImageSettings <|
                        Request.postImageSettings (Zipper.toList filmRoll)
                    )

                SaveAfter t settings ->
                    ( { model | saveStatus = SaveAfter (t - 1) settings }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        GotFilmRollSettings (Ok ( settings, workingDirectory )) ->
            ( { model
                | workingDirectory = Just workingDirectory
                , filmRoll =
                    Maybe.andThen (Zipper.findFirst (\x -> x.iFilename == model.route.filename)) <|
                        Zipper.fromList (List.sortBy .iFilename settings)
              }
            , Task.attempt GotImageDimensions <|
                Browser.Dom.getElement "image-section"
            )

        GotFilmRollSettings (Err err) ->
            ( model, Cmd.none )

        GotHistogram result ->
            ( { model | histogram = Result.withDefault [] result }, Cmd.none )

        GotSaveImageSettings _ ->
            ( model, Cmd.none )

        GotGenerateHighres _ ->
            ( model, Cmd.none )

        GotGeneratePreviews _ ->
            ( model, Cmd.none )

        GotImageDimensions result ->
            ( { model
                | imageWidth =
                    Result.toMaybe <|
                        Result.map (\v -> floor v.element.width) result
              }
            , Cmd.none
            )

        Rotate ->
            ( updateSettings (\s -> { s | iRotate = s.iRotate + degrees -90 }) model, Cmd.none )

        OnImageSettingsChange settings ->
            ( updateSettings (\_ -> settings) model, Cmd.none )

        CopySettings settings ->
            ( { model | clipboard = Just settings }, Cmd.none )

        OnImageLoad settings previewWidth ->
            let
                getHistogram =
                    Cmd.map GotHistogram <|
                        Request.postImageSettingsHistogram previewWidth settings
            in
            case model.imageProcessingState of
                Ready ->
                    ( { model | imageProcessingState = Ready }, Cmd.none )

                Loading ->
                    ( { model | imageProcessingState = Ready }, getHistogram )

                Queued n ->
                    if n == model.filmRoll then
                        ( { model | imageProcessingState = Ready }, getHistogram )

                    else
                        ( { model | imageProcessingState = Loading, filmRoll = n }, Cmd.none )

        SaveSettings filmRoll ->
            ( model
            , Cmd.map GotSaveImageSettings <|
                Request.postImageSettings (Zipper.toList filmRoll)
            )

        GenerateHighres settings ->
            ( model
            , Cmd.map GotGenerateHighres <|
                Request.postImageSettingsHighres settings
            )

        GeneratePreviews ->
            ( model
            , Cmd.map GotGeneratePreviews Request.postImageSettingsPreviews
            )

        UpdateImageCropMode mode ->
            ( { model | imageCropMode = mode }, Cmd.none )

        ApplyCrop imageCrop ->
            ( updateSettings (\s -> { s | iCrop = imageCrop })
                { model | imageCropMode = Nothing }
            , Cmd.none
            )

        PreviousImage ->
            let
                filename =
                    Maybe.map (\z -> Maybe.withDefault (Zipper.last z) (Zipper.previous z)) model.filmRoll
                        |> Maybe.map (.iFilename << Zipper.current)
                        -- FIXME: Don't default
                        |> Maybe.withDefault ""
            in
            ( { model | undoState = [] }
            , Cmd.batch
                [ Navigation.pushUrl model.key <|
                    toUrl filename
                , Maybe.withDefault Cmd.none <|
                    Maybe.map (Cmd.map GotSaveImageSettings << Request.postImageSettings << Zipper.toList)
                        model.filmRoll
                ]
            )

        NextImage ->
            let
                filename =
                    Maybe.map (\z -> Maybe.withDefault (Zipper.first z) (Zipper.next z)) model.filmRoll
                        |> Maybe.map (.iFilename << Zipper.current)
                        -- FIXME: Don't default
                        |> Maybe.withDefault ""
            in
            ( { model | undoState = [] }
            , Cmd.batch
                [ Navigation.pushUrl model.key <|
                    toUrl filename
                , Maybe.withDefault Cmd.none <|
                    Maybe.map (Cmd.map GotSaveImageSettings << Request.postImageSettings << Zipper.toList)
                        model.filmRoll
                ]
            )

        Undo ->
            case model.undoState of
                [] ->
                    ( model, Cmd.none )

                x :: xs ->
                    ( { model | undoState = xs, filmRoll = Just x }
                    , Cmd.none
                    )


updateSettings : (ImageSettings -> ImageSettings) -> Model -> Model
updateSettings f model =
    let
        mapCurrent =
            Maybe.map (Zipper.mapCurrent f)

        maybeCons xs =
            Maybe.withDefault xs << Maybe.map (\x -> x :: xs)
    in
    case model.imageProcessingState of
        Ready ->
            { model
                | imageProcessingState = Loading
                , filmRoll = mapCurrent model.filmRoll
                , undoState = maybeCons model.undoState model.filmRoll
            }

        Loading ->
            { model
                | imageProcessingState = Queued (mapCurrent model.filmRoll)
                , undoState = maybeCons model.undoState model.filmRoll
            }

        Queued n ->
            { model
                | imageProcessingState = Queued (mapCurrent n)
                , undoState = maybeCons model.undoState model.filmRoll
            }



-- VIEW


view : Model -> Browser.Document Msg
view model =
    case Maybe.map2 Tuple.pair model.filmRoll model.workingDirectory of
        Nothing ->
            { title = "Positive"
            , body = [ main_ [] [ div [ class "loading-spinner" ] [] ] ]
            }

        Just ( filmRoll, workingDirectory ) ->
            { title = model.route.filename ++ " | Positive"
            , body =
                [ main_ []
                    [ viewLoading model.imageProcessingState
                    , viewImage filmRoll model
                    , viewSettings filmRoll workingDirectory model
                    , viewFileBrowser workingDirectory filmRoll
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


viewFileBrowser : WorkingDirectory -> FilmRoll -> Html Msg
viewFileBrowser dir filmRoll =
    section [ id "files" ]
        [ ul [] <|
            List.concat
                [ List.map (viewFileLink "" dir.unWorkingDirectory) (Zipper.before filmRoll)
                , [ viewFileLink "-current" dir.unWorkingDirectory (Zipper.current filmRoll) ]
                , List.map (viewFileLink "" dir.unWorkingDirectory) (Zipper.after filmRoll)
                ]
        ]


viewFileLink : String -> String -> ImageSettings -> Html Msg
viewFileLink className dir imageSettings =
    let
        previewExtension x =
            String.dropRight 3 x ++ "jpg"
    in
    li [ class className ]
        [ a
            [ href <|
                toUrl imageSettings.iFilename
            ]
            [ img
                [ src <|
                    Url.Builder.absolute
                        [ dir, "previews", previewExtension imageSettings.iFilename ]
                        []
                ]
                []
            , text imageSettings.iFilename
            ]
        ]


toUrl : String -> String
toUrl filename =
    Url.Builder.absolute [] [ Url.Builder.string "filename" filename ]



-- IMAGE SETTINGS


viewSettings : FilmRoll -> WorkingDirectory -> Model -> Html Msg
viewSettings filmRoll workingDirectory model =
    let
        settings =
            case model.imageProcessingState of
                Queued (Just queuedFilmRoll) ->
                    Zipper.current queuedFilmRoll

                _ ->
                    Zipper.current filmRoll
    in
    section [ id "image-settings" ]
        [ viewSettingsGroup
            [ viewHistogram model.histogram ]
        , viewSettingsGroup <|
            List.map (Html.map OnImageSettingsChange)
                [ viewRangeInput (\v -> { settings | iGamma = v }) 0.1 ( 0, 10, 2.2 ) "Gamma" settings.iGamma
                , viewRangeInput (\v -> { settings | iZone1 = v }) 0.01 ( -0.5, 0.5, 0 ) "Zone I" settings.iZone1
                , viewRangeInput (\v -> { settings | iZone5 = v }) 0.01 ( -0.5, 0.5, 0 ) "Zone V" settings.iZone5
                , viewRangeInput (\v -> { settings | iZone9 = v }) 0.01 ( -0.5, 0.5, 0 ) "Zone IX" settings.iZone9
                , viewRangeInput (\v -> { settings | iBlackpoint = v }) 0.01 ( -0.5, 0.5, 0 ) "Blackpoint" settings.iBlackpoint
                , viewRangeInput (\v -> { settings | iWhitepoint = v }) 0.01 ( 0.5, 1.5, 1 ) "Whitepoint" settings.iWhitepoint
                ]
        , viewSettingsGroup
            [ viewImageCropMode settings model.imageCropMode
            , button [ onClick Rotate ] [ text "Rotate" ]
            ]
        , viewSettingsGroup
            [ button [ onClick (CopySettings settings) ] [ text "Copy settings" ]
            , viewMaybe model.clipboard <|
                \clipboard ->
                    div [ class "image-settings-paste" ]
                        [ label [] [ text (interpolate "Paste settings from: {0}" [ clipboard.iFilename ]) ]
                        , viewClipboardButton "All" { clipboard | iFilename = settings.iFilename }
                        , viewClipboardButton "Tone"
                            { clipboard
                                | iFilename = settings.iFilename
                                , iRotate = settings.iRotate
                                , iCrop = settings.iCrop
                            }
                        , viewClipboardButton "Crop" { settings | iCrop = clipboard.iCrop }
                        ]
            ]
        , viewSettingsGroup
            [ button [ onClick (SaveSettings filmRoll) ] [ text "Save" ]
            , button [ onClick (OnImageSettingsChange (resetAll settings)) ] [ text "Reset" ]
            , button [ onClick (OnImageSettingsChange (resetTone settings)) ] [ text "Reset tone" ]
            ]
        , viewSettingsGroup
            [ button [ onClick (GenerateHighres settings) ] [ text "Generate highres" ]
            , button [ onClick GeneratePreviews ] [ text "Generate previews" ]
            , viewIf (not (List.isEmpty model.undoState)) <|
                \_ -> button [ onClick Undo ] [ text "Undo" ]
            ]
        , viewSettingsGroup
            [ button [ onClick PreviousImage ] [ text "⯇" ]
            , button [ onClick NextImage ] [ text "⯈" ]
            ]
        , pre [ class "info" ]
            [ text <|
                interpolate "{0} | {1}"
                    [ settings.iFilename
                    , workingDirectory.unWorkingDirectory
                    ]
            ]
        ]


viewSettingsGroup : List (Html Msg) -> Html Msg
viewSettingsGroup =
    div [ class "image-settings-group" ]


viewClipboardButton : String -> ImageSettings -> Html Msg
viewClipboardButton title settings =
    button [ onClick (OnImageSettingsChange settings) ] [ text title ]


viewImageCropMode : ImageSettings -> Maybe ImageCrop -> Html Msg
viewImageCropMode current imageCropMode =
    let
        onTopChange imageCrop v =
            UpdateImageCropMode (Just { imageCrop | icTop = v })

        onLeftChange imageCrop v =
            UpdateImageCropMode (Just { imageCrop | icLeft = v })

        onWidthChange imageCrop v =
            UpdateImageCropMode (Just { imageCrop | icWidth = v })
    in
    case imageCropMode of
        Nothing ->
            span [] <|
                [ button [ onClick (UpdateImageCropMode (Just current.iCrop)) ] [ text "Crop" ]
                ]

        Just imageCrop ->
            div [ class "crop-settings" ] <|
                [ viewRangeInput (onTopChange imageCrop) 0.01 ( 0, 5, 0 ) "Top" imageCrop.icTop
                , viewRangeInput (onLeftChange imageCrop) 0.01 ( 0, 5, 0 ) "Left" imageCrop.icLeft
                , viewRangeInput (onWidthChange imageCrop) 0.1 ( 85, 100, 100 ) "Width" imageCrop.icWidth
                , button [ onClick (UpdateImageCropMode Nothing) ] [ text "Cancel" ]
                , button [ onClick (ApplyCrop imageCrop) ] [ text "Apply" ]
                ]


viewRangeInput : (Float -> msg) -> Float -> ( Float, Float, Float ) -> String -> Float -> Html msg
viewRangeInput toMsg stepSize ( min, max, startingValue ) title val =
    let
        deDupe v =
            if v == val then
                Decode.fail "Is same as val"

            else
                Decode.succeed (toMsg v)
    in
    div
        [ class "range-slider"
        , on "dblclick" <|
            Decode.andThen deDupe (Decode.succeed startingValue)
        ]
        [ label [] [ span [] [ text title ], span [] [ text (String.fromFloat val) ] ]
        , input
            [ type_ "range"
            , value (String.fromFloat val)
            , step (String.fromFloat stepSize)
            , Attributes.min (String.fromFloat min)
            , Attributes.max (String.fromFloat max)
            , on "input" <|
                Decode.andThen deDupe <|
                    Decode.at [ "target", "valueAsNumber" ] Decode.float
            ]
            []
        ]


viewImage : FilmRoll -> Model -> Html Msg
viewImage filmRoll model =
    let
        currentCrop =
            .iCrop <|
                Zipper.current filmRoll

        ifCropping settings =
            Maybe.withDefault settings <|
                Maybe.map (\_ -> { settings | iRotate = 0, iCrop = ImageCrop 0 0 100 })
                    model.imageCropMode
    in
    section [ id "image-section" ]
        [ div [ class "image-wrapper" ]
            [ viewMaybe model.imageWidth <|
                \imageWidth ->
                    img
                        [ on "load" (Decode.succeed (OnImageLoad (Zipper.current filmRoll) imageWidth))
                        , style "user-select" "none"
                        , src <|
                            Url.Builder.absolute [ "image" ]
                                [ toImageUrlParams (ifCropping (Zipper.current filmRoll))
                                , Url.Builder.int "preview-width" imageWidth
                                ]
                        ]
                        []
            , viewMaybe model.imageCropMode <|
                \imageCrop ->
                    div
                        [ class "image-crop-overlay"
                        , style "transform" <|
                            interpolate "translate({0}%, {1}%)"
                                [ String.fromFloat imageCrop.icLeft
                                , String.fromFloat imageCrop.icTop
                                ]
                        , style "width" <|
                            interpolate "{0}%"
                                [ String.fromFloat imageCrop.icWidth
                                ]
                        ]
                        []
            ]
        , viewZones filmRoll model
        ]


viewHistogram : List Int -> Html msg
viewHistogram =
    Html.Keyed.node "div" [ class "histogram" ] << List.indexedMap viewHistogramBar


viewHistogramBar : Int -> Int -> ( String, Html msg )
viewHistogramBar index v =
    ( String.fromInt index
    , span
        [ class "histogram-bar"
        , style "height" <|
            interpolate "{0}px" [ String.fromFloat (toFloat v / 400) ]
        ]
        []
    )



-- ZONES


viewZones : FilmRoll -> Model -> Html Msg
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


resetAll : ImageSettings -> ImageSettings
resetAll current =
    ImageSettings current.iFilename 0 (ImageCrop 0 0 100) 2.2 0 0 0 0 1


resetTone : ImageSettings -> ImageSettings
resetTone current =
    ImageSettings current.iFilename current.iRotate current.iCrop 2.2 0 0 0 0 1


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
