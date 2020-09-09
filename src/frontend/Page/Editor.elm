module Page.Editor exposing
    ( Model
    , Msg
    , init
    , subscriptions
    , update
    , view
    )

import Base64
import Browser.Events
import Browser.Navigation as Navigation
import Dict exposing (Dict)
import Generated.Data.ImageSettings as ImageSettings
    exposing
        ( FilmRollSettings
        , ImageCrop
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
import Process
import Set exposing (Set)
import String.Interpolate exposing (interpolate)
import Task
import Url exposing (Url)
import Url.Builder
import Util exposing (..)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions { imageCropMode, route, clipboard, filmRoll } =
    let
        current =
            Zipper.current filmRoll
    in
    Sub.batch
        [ Browser.Events.onKeyDown <|
            Decode.oneOf
                [ matchKey "s" (SaveSettings route.dir filmRoll)
                , matchKey "c" (CopySettings (Zipper.current filmRoll))
                , matchKey "r" Rotate
                , matchKey "h" (PreviousImage route.dir filmRoll)
                , matchKey "l" (NextImage route.dir filmRoll)
                , matchKey "8" (SetPreviewScale 8)
                , matchKey "9" (SetPreviewScale 9)
                ]
        , Maybe.withDefault Sub.none <|
            Maybe.map
                (\clipboard_ ->
                    Browser.Events.onKeyDown <|
                        withCtrl <|
                            Decode.map OnImageSettingsChange <|
                                Decode.oneOf
                                    [ matchKey "a" { clipboard_ | iFilename = current.iFilename }
                                    , matchKey "c" { current | iCrop = clipboard_.iCrop }
                                    , matchKey "t"
                                        { clipboard_
                                            | iFilename = current.iFilename
                                            , iRotate = current.iRotate
                                            , iCrop = current.iCrop
                                        }
                                    ]
                )
                clipboard
        , Maybe.withDefault Sub.none <|
            Maybe.map
                (always <|
                    Browser.Events.onKeyDown <|
                        matchKey "Escape" (UpdateImageCropMode Nothing)
                )
                imageCropMode
        ]



-- MODEL


type alias Model =
    { imageProcessingState : ImageProcessingState
    , filmRoll : FilmRoll
    , saveKey : Key { saveKey : () }
    , imageCropMode : Maybe ImageCrop
    , clipboard : Maybe ImageSettings
    , histogram : List Int
    , undoState : List FilmRoll
    , scale : Float
    , previewColumns : Int
    , route : Route
    }


type alias FilmRoll =
    Zipper ImageSettings


type ImageProcessingState
    = Ready
    | Processing
    | Queued FilmRoll


init : Route -> FilmRoll -> Model
init route filmRoll =
    { imageProcessingState = Ready
    , filmRoll = filmRoll
    , saveKey = Key 0
    , imageCropMode = Nothing
    , clipboard = Nothing
    , histogram = []
    , undoState = []
    , scale = 1
    , previewColumns = 5
    , route = route
    }



-- UPDATE


type alias HttpResult a =
    Result ( Http.Error, Maybe { metadata : Http.Metadata, body : String } ) a


type Msg
    = GotSaveImageSettings String (HttpResult FilmRollSettings)
    | GotGenerateHighres (HttpResult ())
    | GotHistogram (HttpResult (List Int))
    | RotatePreview String Float
    | OnImageSettingsChange ImageSettings
    | OnImageLoad String ImageSettings
    | SaveSettings String FilmRoll
    | GenerateHighres String ImageSettings
    | CopySettings ImageSettings
    | ApplyCopyToAll FilmRoll
    | UpdateImageCropMode (Maybe ImageCrop)
    | ApplyCrop ImageCrop
    | PreviousImage String FilmRoll
    | NextImage String FilmRoll
    | Undo
    | UpdateScale Float
    | SetPreviewScale Int
    | AttemptSave String (Key { saveKey : () }) FilmRoll
    | ToggleStar String String FilmRoll
    | Rotate


update : Navigation.Key -> Msg -> Model -> ( Model, Cmd Msg )
update key msg model =
    case msg of
        GotHistogram result ->
            ( { model | histogram = Result.withDefault [] result }, Cmd.none )

        GotSaveImageSettings dir (Ok _) ->
            -- pushNotification "Saved settings" model
            ( model, Cmd.none )

        GotSaveImageSettings _ (Err _) ->
            -- pushNotification "Error saving settings" model
            ( model, Cmd.none )

        GotGenerateHighres (Ok _) ->
            -- pushNotification "Generated highres" model
            ( model, Cmd.none )

        GotGenerateHighres (Err _) ->
            -- pushNotification "Error generating highres" model
            ( model, Cmd.none )

        Rotate ->
            ( updateSettings
                (\s ->
                    { s
                        | iRotate = fractionalModBy (degrees -360) (s.iRotate - degrees 90)
                    }
                )
                model
            , Cmd.none
            )

        RotatePreview filename rotation ->
            let
                filmRoll =
                    Zipper.map
                        (\s ->
                            if s.iFilename == filename then
                                { s | iRotate = rotation }

                            else
                                s
                        )
                        model.filmRoll
            in
            ( { model | filmRoll = filmRoll, saveKey = nextKey model.saveKey }
            , Cmd.none
            )

        OnImageSettingsChange settings ->
            ( updateSettings (\_ -> settings) model, Cmd.none )

        CopySettings settings ->
            ( { model | clipboard = Just settings }, Cmd.none )

        ApplyCopyToAll filmRoll ->
            let
                isLoading =
                    (\f -> Zipper.current f /= Zipper.current filmRoll) model.filmRoll
            in
            if isLoading then
                ( { model | imageProcessingState = Processing, filmRoll = filmRoll }, Cmd.none )

            else
                ( { model | filmRoll = filmRoll }, Cmd.none )

        OnImageLoad dir settings ->
            let
                getHistogram =
                    Cmd.map GotHistogram <|
                        Request.postImageSettingsHistogram (Url.percentEncode dir) settings
            in
            case model.imageProcessingState of
                Ready ->
                    ( { model | imageProcessingState = Ready }, Cmd.none )

                Processing ->
                    ( { model | imageProcessingState = Ready }, getHistogram )

                Queued n ->
                    if n == model.filmRoll then
                        ( { model | imageProcessingState = Ready }, getHistogram )

                    else
                        ( { model | imageProcessingState = Processing, filmRoll = n }, Cmd.none )

        SaveSettings dir filmRoll ->
            ( model
              -- , Cmd.map (GotSaveImageSettings dir) <|
              --     Request.postImageSettings (Url.percentEncode dir) <|
              --         fromZipper (Dict.get dir model.posters) (Dict.get dir model.starred) filmRoll
            , Cmd.none
            )

        GenerateHighres dir settings ->
            -- pushNotification "Generating highres version" model
            --     |> Tuple.mapSecond
            --         (\cmds ->
            --             Cmd.batch
            --                 [ Cmd.map GotGenerateHighres (Request.postImageSettingsHighres (Url.percentEncode dir) settings)
            --                 , cmds
            --                 ]
            --         )
            ( model, Cmd.none )

        UpdateImageCropMode mode ->
            ( { model | imageCropMode = mode }, Cmd.none )

        ApplyCrop imageCrop ->
            ( updateSettings (\s -> { s | iCrop = imageCrop })
                { model | imageCropMode = Nothing }
            , Cmd.none
            )

        PreviousImage dir filmRoll ->
            ( { model | undoState = [], saveKey = nextKey model.saveKey }
            , Cmd.batch
                [ Navigation.pushUrl key <|
                    (Util.toUrl << Route dir << .iFilename << Zipper.current) <|
                        Maybe.withDefault (Zipper.last filmRoll) (Zipper.previous filmRoll)
                , Task.perform (\_ -> AttemptSave dir (nextKey model.saveKey) filmRoll) <|
                    Process.sleep 10000
                ]
            )

        NextImage dir filmRoll ->
            ( { model | undoState = [], saveKey = nextKey model.saveKey }
            , Cmd.batch
                [ Navigation.pushUrl key <|
                    (Util.toUrl << Route dir << .iFilename << Zipper.current) <|
                        Maybe.withDefault (Zipper.first filmRoll) (Zipper.next filmRoll)
                , Task.perform (\_ -> AttemptSave dir (nextKey model.saveKey) filmRoll) <|
                    Process.sleep 10000
                ]
            )

        Undo ->
            case model.undoState of
                [] ->
                    ( model, Cmd.none )

                x :: xs ->
                    ( { model | undoState = xs, filmRoll = x }
                    , Cmd.none
                    )

        UpdateScale val ->
            ( { model | scale = val }, Cmd.none )

        SetPreviewScale scale ->
            ( { model | previewColumns = scale }, Cmd.none )

        AttemptSave dir saveKey filmRoll ->
            if saveKey /= model.saveKey then
                ( model, Cmd.none )

            else
                ( model
                  -- , Cmd.map (GotSaveImageSettings dir) <|
                  --     Request.postImageSettings (Url.percentEncode dir) <|
                  --         fromZipper (Dict.get dir model.posters) (Dict.get dir model.starred) filmRoll
                , Cmd.none
                )

        ToggleStar dir filename filmRoll ->
            let
                toggle v =
                    Just <|
                        if Set.member filename v then
                            Set.remove filename v

                        else
                            Set.insert filename v
            in
            ( model
            , Cmd.none
            )


type Key a
    = Key Int


nextKey : Key a -> Key a
nextKey (Key k) =
    Key (k + 1)


updateSettings : (ImageSettings -> ImageSettings) -> Model -> Model
updateSettings f model =
    case model.imageProcessingState of
        Ready ->
            { model
                | imageProcessingState = Processing
                , filmRoll = Zipper.mapCurrent f model.filmRoll
                , undoState = model.filmRoll :: model.undoState
            }

        Processing ->
            { model
                | imageProcessingState = Queued (Zipper.mapCurrent f model.filmRoll)
                , undoState = model.filmRoll :: model.undoState
            }

        Queued n ->
            { model
                | imageProcessingState = Queued (Zipper.mapCurrent f n)
                , undoState = model.filmRoll :: model.undoState
            }


fromZipper : Maybe String -> Maybe (Set String) -> FilmRoll -> FilmRollSettings
fromZipper poster starred =
    FilmRollSettings poster (Maybe.withDefault Set.empty starred)
        << Dict.fromList
        << List.map (\x -> ( x.iFilename, x ))
        << Zipper.toList



-- VIEW


view : Model -> Html Msg
view model =
    main_ []
        [ viewNav model.route
        , viewLoading model.imageProcessingState
        , viewImage model.filmRoll model.route model
        , viewSettings model.filmRoll model.route model
        , viewCurrentFilmRoll model.route model.previewColumns model.filmRoll
        ]



-- NAV


viewNav : Route -> Html Msg
viewNav route =
    nav []
        [ a [ href "/" ] [ text "browser" ]
        , text "/"
        , text route.dir
        , text "/"
        , text route.filename
        ]



-- FILES


viewCurrentFilmRoll : Route -> Int -> FilmRoll -> Html Msg
viewCurrentFilmRoll route columns filmRoll =
    section [ class "files" ]
        [ viewRangeInput (SetPreviewScale << floor) 1 ( 2, 13, 5 ) "Columns" (toFloat columns) -- FIXME: remove floats
        , ul [] <|
            List.concat
                [ List.map (viewCurrentFilmRollLink False route.dir columns) (Zipper.before filmRoll)
                , [ viewCurrentFilmRollLink True route.dir columns (Zipper.current filmRoll) ]
                , List.map (viewCurrentFilmRollLink False route.dir columns) (Zipper.after filmRoll)
                ]
        ]


viewCurrentFilmRollLink : Bool -> String -> Int -> ImageSettings -> Html Msg
viewCurrentFilmRollLink isCurrent dir columns settings =
    let
        previewExtension x =
            String.dropRight 3 x ++ "jpg"

        width =
            style "width" <|
                interpolate "calc({0}% - 1rem)" [ String.fromInt (100 // columns) ]

        rotate deg =
            fractionalModBy (degrees -360) (settings.iRotate - degrees deg)
    in
    li [ classList [ ( "-current", isCurrent ), ( "-small", columns > 5 ) ], width ]
        [ a
            [ href <|
                Util.toUrl { filename = settings.iFilename, dir = dir }
            ]
            [ img
                [ src <|
                    Url.Builder.absolute
                        [ dir, "previews", previewExtension settings.iFilename ]
                        []
                ]
                []
            ]
        , span [ class "files-file-rotate" ]
            [ button [ onClick (RotatePreview settings.iFilename (rotate 270)) ] [ text "⇥" ]
            , button [ onClick (RotatePreview settings.iFilename (rotate 180)) ] [ text "⇥" ]
            , button [ onClick (RotatePreview settings.iFilename (rotate 90)) ] [ text "⇥" ]
            ]
        , span [ class "files-file-footer" ]
            [ text settings.iFilename
            , button [ onClick (CopySettings settings) ] [ text "copy" ]

            -- , button [ onClick (ToggleStar dir settings.iFilename) ] [ text "star" ]
            ]
        ]



-- IMAGE SETTINGS


viewSettings : FilmRoll -> Route -> Model -> Html Msg
viewSettings filmRoll route model =
    let
        settings =
            case model.imageProcessingState of
                Queued queuedFilmRoll ->
                    Zipper.current queuedFilmRoll

                _ ->
                    Zipper.current filmRoll
    in
    section [ class "image-settings" ]
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
                        [ pre [] [ text (interpolate "Paste settings from: {0}" [ clipboard.iFilename ]) ]
                        , viewClipboardButton "Both" { clipboard | iFilename = settings.iFilename }
                        , viewClipboardButton "Tone"
                            { clipboard
                                | iFilename = settings.iFilename
                                , iRotate = settings.iRotate
                                , iCrop = settings.iCrop
                            }
                        , viewClipboardButton "Crop" { settings | iCrop = clipboard.iCrop }
                        , button
                            [ onClick <|
                                ApplyCopyToAll <|
                                    Zipper.map (\i -> { clipboard | iFilename = i.iFilename }) filmRoll
                            ]
                            [ text "Apply to all" ]
                        ]
            ]
        , viewSettingsGroup
            [ button [ onClick (SaveSettings route.dir filmRoll) ] [ text "Save" ]
            , button [ onClick (OnImageSettingsChange (resetAll settings)) ] [ text "Reset" ]
            , button [ onClick (OnImageSettingsChange (resetTone settings)) ] [ text "Reset tone" ]
            ]
        , viewSettingsGroup
            [ button [ onClick (GenerateHighres route.dir settings) ] [ text "Generate highres" ]
            , viewIf (not (List.isEmpty model.undoState)) <|
                \_ -> button [ onClick Undo ] [ text "Undo" ]
            ]
        , viewSettingsGroup
            [ button [ onClick (PreviousImage route.dir filmRoll) ] [ text "⯇" ]
            , button [ onClick (NextImage route.dir filmRoll) ] [ text "⯈" ]
            ]

        -- , pre [ class "info" ]
        --     [ text <|
        --         interpolate "{0} | {1}"
        --             [ settings.iFilename
        --             , route.dir
        --             ]
        -- ]
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


viewImage : FilmRoll -> Route -> Model -> Html Msg
viewImage filmRoll route model =
    let
        current =
            Zipper.current filmRoll

        currentCrop =
            .iCrop <|
                Zipper.current filmRoll

        ifCropping settings =
            Maybe.withDefault settings <|
                Maybe.map (\_ -> { settings | iRotate = 0, iCrop = ImageCrop 0 0 100 })
                    model.imageCropMode

        scale =
            style "transform" ("scale(" ++ String.fromFloat model.scale ++ ")")
    in
    section [ id "image-section" ]
        [ div [ class "image-wrapper", scale ]
            [ img
                [ on "load" (Decode.succeed (OnImageLoad route.dir (Zipper.current filmRoll)))
                , style "user-select" "none"
                , src <|
                    Url.Builder.absolute [ "image" ]
                        [ toImageUrlParams (ifCropping (Zipper.current filmRoll))
                        , Url.Builder.string "dir" route.dir
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
            interpolate "{0}px" [ String.fromFloat (toFloat v / 300) ]
        ]
        []
    )



-- NOTIFICATONS


viewNotification : List String -> Html msg
viewNotification notifications =
    div [ class "notifications" ] <|
        List.map (\x -> span [] [ text x ]) (List.reverse notifications)


viewLoading : ImageProcessingState -> Html msg
viewLoading state =
    viewIf (state /= Ready) <|
        \_ ->
            div [ class "loading-spinner" ] []



-- ZONES


viewZones : FilmRoll -> Model -> Html Msg
viewZones filmRoll model =
    let
        settings =
            Zipper.current filmRoll

        zone t x v =
            v + (x * m t v)

        m t v =
            (1 - abs (v - t)) ^ 2

        vs =
            List.map (\x -> toFloat x / 10) <| List.range 1 10

        apply =
            zone 0.9 settings.iZone9 << zone 0.5 settings.iZone5 << zone 0.1 settings.iZone1
    in
    section [ class "zones" ]
        [ viewRangeInput UpdateScale 0.01 ( 0.05, 1.05, 1 ) "Zoom" model.scale
        , Html.Keyed.ul [] <|
            List.map2 viewZoneBar (List.map apply vs) vs
        ]


viewZoneBar : Float -> Float -> ( String, Html msg )
viewZoneBar value zone =
    let
        height v =
            style "height" (String.fromFloat (abs (1 + 4 * v)) ++ "rem")

        background v =
            style "background-color" ("hsl(0, 0%," ++ String.fromFloat (v * 100) ++ "%)")
    in
    ( String.fromFloat value
    , li [ height (value - zone), background zone ]
        []
      -- [ span [] [ text (String.left 6 (String.fromFloat value)) ] ]
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


fractionalModBy : Float -> Float -> Float
fractionalModBy m v =
    v - m * Basics.toFloat (Basics.floor (v / m))


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
