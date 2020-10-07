port module Page.Editor exposing
    ( Model
    , Msg
    , continue
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
        , Zones
        )
import Generated.Request as Request
import Html exposing (..)
import Html.Attributes as Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed
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



-- PORT


port previewReady : (String -> msg) -> Sub msg



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions { imageCropMode, route, clipboard, filmRoll } =
    let
        current =
            Zipper.current filmRoll

        maybe f =
            Maybe.withDefault Sub.none << Maybe.map (Browser.Events.onKeyDown << f)
    in
    Sub.batch
        [ Browser.Events.onKeyDown <|
            Decode.oneOf
                [ matchKey "s" SaveSettings
                , matchKey "u" Undo
                , matchKey "c" (CopySettings current)
                , matchKey "r" Rotate
                , matchKey "h" PreviousImage
                , matchKey "l" NextImage
                , matchKey "3" (UpdateScale 0.3)
                , matchKey "4" (UpdateScale 0.4)
                , matchKey "5" (UpdateScale 0.5)
                , matchKey "6" (UpdateScale 0.6)
                , matchKey "7" (UpdateScale 0.7)
                , matchKey "8" (UpdateScale 0.8)
                , matchKey "9" (UpdateScale 0.9)
                ]
        , maybe
            (\clipboard_ ->
                Decode.map OnImageSettingsChange <|
                    withCtrl <|
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
        , maybe
            (always <|
                matchKey "Escape" (UpdateImageCropMode Nothing)
            )
            imageCropMode
        , previewReady OnPreviewReady
        ]



-- MODEL


type alias Model =
    { imageProcessingState : ImageProcessingState
    , ratings : Ratings
    , previewVersions : PreviewVersions
    , poster : Maybe String
    , filmRoll : FilmRoll
    , saveKey : Key { saveKey : () }
    , imageCropMode : Maybe ImageCrop
    , clipboard : Maybe ImageSettings
    , histogram : List Int
    , undoState : List FilmRoll
    , scale : Float
    , minimumRating : Int
    , previewColumns : Int
    , route : Route
    , notifications : List ( Level, String )
    }


type alias FilmRoll =
    Zipper ImageSettings


type alias Ratings =
    Dict String Int


type alias PreviewVersions =
    Dict String Int


type ImageProcessingState
    = Ready
    | Processing
    | Queued FilmRoll
    | Preview


init : Route -> FilmRoll -> Ratings -> Maybe String -> Model
init route filmRoll ratings poster =
    { imageProcessingState = Preview
    , ratings = ratings
    , poster = poster
    , filmRoll = focus route filmRoll
    , saveKey = Key 0
    , imageCropMode = Nothing
    , clipboard = Nothing
    , histogram = []
    , undoState = []
    , scale = 1
    , minimumRating = 0
    , previewColumns = 5
    , route = route
    , notifications = []
    , previewVersions = Dict.empty
    }


continue : Route -> FilmRoll -> Ratings -> Maybe String -> Model -> Model
continue route filmRoll ratings poster model =
    { model
        | imageProcessingState = Preview
        , ratings = ratings
        , poster = poster
        , filmRoll = focus route filmRoll
    }


focus : Route -> FilmRoll -> FilmRoll
focus route filmRoll =
    Maybe.withDefault filmRoll <|
        Zipper.findFirst ((==) route.filename << .iFilename) filmRoll



-- UPDATE


type Msg
    = GotSaveImageSettings String (HttpResult FilmRollSettings)
    | GotGenerate String (HttpResult ())
    | GotHistogram (HttpResult (List Int))
    | RotatePreview String Float
    | OnImageSettingsChange ImageSettings
    | OnImageLoad String ImageSettings
    | SaveSettings
    | GenerateHighres
    | GenerateWallpaper
    | CopySettings ImageSettings
    | ApplyCopyToAll FilmRoll
    | UpdateImageCropMode (Maybe ImageCrop)
    | ApplyCrop ImageCrop
    | PreviousImage
    | NextImage
    | Undo
    | UpdateScale Float
    | SetColumnCount Int
    | AttemptSave String (Key { saveKey : () }) FilmRoll
    | Rate String Int
    | SetMinRating Int
    | Rotate
    | RemoveNotification
    | OnPreviewReady String
    | LoadOriginal


update : Navigation.Key -> Msg -> Model -> ( Model, Cmd Msg )
update key msg model =
    case msg of
        GotHistogram result ->
            ( { model | histogram = Result.withDefault [] result }, Cmd.none )

        GotSaveImageSettings dir (Ok _) ->
            pushNotification Normal RemoveNotification "Saved settings" model

        GotSaveImageSettings _ (Err _) ->
            pushNotification Warning RemoveNotification "Error saving settings" model

        GotGenerate type_ (Ok _) ->
            pushNotification Normal RemoveNotification ("Generated " ++ type_) model

        GotGenerate type_ (Err _) ->
            pushNotification Warning RemoveNotification ("Error generating " ++ type_) model

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
                    Zipper.current model.filmRoll /= Zipper.current filmRoll
            in
            if isLoading then
                ( { model | imageProcessingState = Processing, filmRoll = filmRoll }, Cmd.none )

            else
                ( { model | filmRoll = filmRoll }, Cmd.none )

        OnImageLoad dir settings ->
            let
                getHistogram =
                    Cmd.map GotHistogram <|
                        Request.postImageSettingsHistogram dir settings
            in
            case model.imageProcessingState of
                Preview ->
                    ( { model | imageProcessingState = Ready }, Cmd.none )

                Ready ->
                    ( { model | imageProcessingState = Ready }, Cmd.none )

                Processing ->
                    ( { model | imageProcessingState = Ready }, getHistogram )

                Queued n ->
                    if n == model.filmRoll then
                        ( { model | imageProcessingState = Ready }, getHistogram )

                    else
                        ( { model | imageProcessingState = Processing, filmRoll = n }, Cmd.none )

        OnPreviewReady filename ->
            let
                f x =
                    case x of
                        Nothing ->
                            Just 1

                        Just y ->
                            Just (y + 1)
            in
            pushNotification Normal
                RemoveNotification
                ("Preview ready: " ++ filename)
                { model | previewVersions = Dict.update filename f model.previewVersions }

        SaveSettings ->
            ( model
            , saveSettings model
            )

        GenerateHighres ->
            pushNotification Normal RemoveNotification "Generating highres version" model
                |> Tuple.mapSecond
                    (\cmds ->
                        Cmd.batch
                            [ Cmd.map (GotGenerate "highres") <|
                                Request.postImageSettingsHighres
                                    model.route.dir
                                    (Zipper.current model.filmRoll)
                            , cmds
                            ]
                    )

        GenerateWallpaper ->
            pushNotification Normal RemoveNotification "Generating wallpaper version" model
                |> Tuple.mapSecond
                    (\cmds ->
                        Cmd.batch
                            [ Cmd.map (GotGenerate "wallpaper") <|
                                Request.postImageSettingsWallpaper
                                    model.route.dir
                                    (Zipper.current model.filmRoll)
                            , cmds
                            ]
                    )

        UpdateImageCropMode mode ->
            ( { model | imageCropMode = mode }, Cmd.none )

        ApplyCrop imageCrop ->
            ( updateSettings (\s -> { s | iCrop = imageCrop })
                { model | imageCropMode = Nothing }
            , Cmd.none
            )

        PreviousImage ->
            ( { model | undoState = [], saveKey = nextKey model.saveKey }
            , Cmd.batch
                [ Navigation.pushUrl key <|
                    (Util.toUrl << Route model.route.dir << .iFilename << Zipper.current) <|
                        Maybe.withDefault (Zipper.last model.filmRoll) (Zipper.previous model.filmRoll)
                , Task.perform (\_ -> AttemptSave model.route.dir (nextKey model.saveKey) model.filmRoll) <|
                    Process.sleep 10000
                ]
            )

        NextImage ->
            ( { model | undoState = [], saveKey = nextKey model.saveKey }
            , Cmd.batch
                [ Navigation.pushUrl key <|
                    (Util.toUrl << Route model.route.dir << .iFilename << Zipper.current) <|
                        Maybe.withDefault (Zipper.first model.filmRoll) (Zipper.next model.filmRoll)
                , Task.perform (\_ -> AttemptSave model.route.dir (nextKey model.saveKey) model.filmRoll) <|
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

        SetColumnCount scale ->
            ( { model | previewColumns = scale }, Cmd.none )

        AttemptSave dir saveKey filmRoll ->
            if saveKey /= model.saveKey then
                ( model, Cmd.none )

            else
                ( model
                , saveSettings model
                )

        Rate filename rating ->
            let
                check x =
                    case x of
                        Nothing ->
                            Just rating

                        Just y ->
                            if y == rating then
                                Nothing

                            else
                                Just rating

                newModel =
                    { model | ratings = Dict.update filename check model.ratings }
            in
            ( newModel
            , saveSettings newModel
            )

        SetMinRating rating ->
            ( { model | minimumRating = rating }, Cmd.none )

        RemoveNotification ->
            ( { model | notifications = List.drop 1 model.notifications }, Cmd.none )

        LoadOriginal ->
            ( { model | imageProcessingState = Processing }, Cmd.none )


type Key a
    = Key Int


nextKey : Key a -> Key a
nextKey (Key k) =
    Key (k + 1)


saveSettings : Model -> Cmd Msg
saveSettings model =
    Cmd.map (GotSaveImageSettings model.route.dir) <|
        Request.postImageSettings model.route.dir <|
            fromZipper model.poster model.ratings model.filmRoll


updateSettings : (ImageSettings -> ImageSettings) -> Model -> Model
updateSettings f model =
    case model.imageProcessingState of
        Preview ->
            { model
                | imageProcessingState = Processing
                , filmRoll = Zipper.mapCurrent f model.filmRoll
                , undoState = model.filmRoll :: model.undoState
            }

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


fromZipper : Maybe String -> Ratings -> FilmRoll -> FilmRollSettings
fromZipper poster ratings =
    FilmRollSettings poster ratings
        << Dict.fromList
        << List.map (\x -> ( x.iFilename, x ))
        << Zipper.toList



-- VIEW


view : Model -> List ( Level, String ) -> Html Msg
view model otherNotifications =
    main_ []
        [ viewNav model.route
        , viewLoading model.imageProcessingState
        , viewImage model.filmRoll model.route model
        , viewSettings model.filmRoll model.route model
        , viewCurrentFilmRoll model.route
            model.previewColumns
            model.minimumRating
            model.previewVersions
            model.ratings
            model.filmRoll
        , viewNotifications (model.notifications ++ otherNotifications)
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


viewCurrentFilmRoll : Route -> Int -> Int -> PreviewVersions -> Ratings -> FilmRoll -> Html Msg
viewCurrentFilmRoll route columns minimumRating previewVersions ratings filmRoll =
    section [ class "files" ]
        [ viewRangeInput (SetColumnCount << floor) 1 ( 2, 13, 5 ) "Columns" (toFloat columns) -- FIXME: remove floats
        , viewRangeInput (SetMinRating << floor) 1 ( 0, 5, 0 ) "Rating" (toFloat minimumRating) -- FIXME: remove floats
        , Html.Keyed.ul [] <|
            List.map (\( _, filename, x ) -> ( filename, x )) <|
                List.filter (\( rating, _, _ ) -> rating >= minimumRating) <|
                    List.concat
                        [ List.map (viewCurrentFilmRollLink False route.dir columns previewVersions ratings) (Zipper.before filmRoll)
                        , [ viewCurrentFilmRollLink True route.dir columns previewVersions ratings (Zipper.current filmRoll) ]
                        , List.map (viewCurrentFilmRollLink False route.dir columns previewVersions ratings) (Zipper.after filmRoll)
                        ]
        ]


viewCurrentFilmRollLink : Bool -> String -> Int -> PreviewVersions -> Ratings -> ImageSettings -> ( Int, String, Html Msg )
viewCurrentFilmRollLink isCurrent dir columns previewVersions ratings settings =
    let
        width =
            style "width" <|
                interpolate "calc({0}% - 1rem)" [ String.fromInt (100 // columns) ]

        rotate deg =
            fractionalModBy (degrees -360) (settings.iRotate - degrees deg)
    in
    ( Maybe.withDefault 0 (Dict.get settings.iFilename ratings)
    , settings.iFilename
    , li [ classList [ ( "-current", isCurrent ), ( "-small", columns > 5 ) ], width ]
        [ a
            [ href <|
                Util.toUrl { filename = settings.iFilename, dir = dir }
            ]
            [ img
                [ src <|
                    Url.Builder.absolute
                        [ dir, "previews", previewExtension settings.iFilename ]
                        [ Url.Builder.int "v" <|
                            Maybe.withDefault 0 (Dict.get settings.iFilename previewVersions)
                        ]
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
            , viewRating settings.iFilename ratings
            ]
        ]
    )


viewRating : String -> Ratings -> Html Msg
viewRating filename ratings =
    let
        rating =
            Maybe.withDefault 0 <|
                Dict.get filename ratings

        gliph n =
            if n <= rating then
                "★"

            else
                "☆"
    in
    div [ class "ratings" ] <|
        List.map
            (\n -> button [ onClick (Rate filename n) ] [ text (gliph n) ])
            (List.range 1 5)



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

        zones =
            settings.iZones
    in
    section [ class "image-settings" ]
        [ viewSettingsGroup
            [ viewHistogram model.histogram ]
        , viewSettingsGroup <|
            List.map (Html.map OnImageSettingsChange)
                [ viewRangeInput (\v -> { settings | iZones = { zones | z1 = v } }) 0.001 ( -0.25, 0.25, 0 ) "I" zones.z1
                , viewRangeInput (\v -> { settings | iZones = { zones | z3 = v } }) 0.001 ( -0.25, 0.25, 0 ) "III" zones.z3
                , viewRangeInput (\v -> { settings | iZones = { zones | z5 = v } }) 0.001 ( -0.25, 0.25, 0 ) "V" zones.z5
                , viewRangeInput (\v -> { settings | iZones = { zones | z7 = v } }) 0.001 ( -0.25, 0.25, 0 ) "VII" zones.z7
                , viewRangeInput (\v -> { settings | iZones = { zones | z9 = v } }) 0.001 ( -0.25, 0.25, 0 ) "IX" zones.z9
                ]
        , viewSettingsGroup <|
            List.map (Html.map OnImageSettingsChange)
                [ viewRangeInput (\v -> { settings | iGamma = v }) 0.1 ( 0, 10, 2.2 ) "Gamma" settings.iGamma
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
            [ button [ onClick SaveSettings ] [ text "Save" ]
            , button [ onClick (OnImageSettingsChange (resetAll settings)) ] [ text "Reset" ]
            , button [ onClick (OnImageSettingsChange (resetTone settings)) ] [ text "Reset tone" ]
            ]
        , viewSettingsGroup
            [ button [ onClick GenerateHighres ] [ text "Generate highres" ]
            , button [ onClick GenerateWallpaper ] [ text "Generate wallpaper" ]
            , viewIf (not (List.isEmpty model.undoState)) <|
                \_ -> button [ onClick Undo ] [ text "Undo" ]
            ]
        , viewSettingsGroup
            [ button [ onClick PreviousImage ] [ text "⯇" ]
            , button [ onClick NextImage ] [ text "⯈" ]
            , viewIf (model.imageProcessingState == Preview) <|
                \_ -> button [ onClick LoadOriginal ] [ text "Load original" ]
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
            [ case model.imageProcessingState of
                Preview ->
                    img
                        [ style "user-select" "none"
                        , src <|
                            Url.Builder.absolute
                                [ route.dir, "previews", previewExtension current.iFilename ]
                                [ Url.Builder.int "v" <|
                                    Maybe.withDefault 0 (Dict.get current.iFilename model.previewVersions)
                                ]
                        ]
                        []

                _ ->
                    img
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
        , section [ class "zones" ]
            [ viewRangeInput UpdateScale 0.01 ( 0.05, 1.05, 1 ) "Zoom" model.scale ]
        ]


viewHistogram : List Int -> Html msg
viewHistogram =
    Html.Keyed.node "div" [ class "histogram" ] << List.indexedMap viewHistogramBar


viewHistogramBar : Int -> Int -> ( String, Html msg )
viewHistogramBar index v =
    ( String.fromInt index
    , span
        [ class "histogram-bar"
        , title (String.fromFloat (toFloat v / 400))
        , style "height" <|
            interpolate "{0}px" [ String.fromFloat (toFloat v / 400) ]
        ]
        []
    )



-- NOTIFICATONS


viewLoading : ImageProcessingState -> Html msg
viewLoading state =
    case state of
        Ready ->
            text ""

        Preview ->
            text ""

        Processing ->
            div [ class "loading-spinner" ] []

        Queued _ ->
            div [ class "loading-spinner" ] []



-- HELPERS


previewExtension : String -> String
previewExtension x =
    String.dropRight 3 x ++ "jpg"


resetAll : ImageSettings -> ImageSettings
resetAll current =
    ImageSettings current.iFilename 0 (ImageCrop 0 0 100) 2.2 (Zones 0 0 0 0 0 0 0 0 0) 0 1


resetTone : ImageSettings -> ImageSettings
resetTone current =
    ImageSettings current.iFilename current.iRotate current.iCrop 2.2 (Zones 0 0 0 0 0 0 0 0 0) 0 1


toImageUrlParams : ImageSettings -> Url.Builder.QueryParameter
toImageUrlParams =
    Url.Builder.string "image-settings"
        << Base64.encode
        << Encode.encode 0
        << ImageSettings.encodeImageSettings


fractionalModBy : Float -> Float -> Float
fractionalModBy m v =
    v - m * Basics.toFloat (Basics.floor (v / m))
