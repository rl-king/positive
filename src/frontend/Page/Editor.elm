port module Page.Editor exposing
    ( Model
    , Msg
    , continue
    , init
    , subscriptions
    , update
    , view
    )

import Array exposing (Array)
import Base64
import Browser.Dom exposing (Element)
import Browser.Events
import Browser.Navigation as Navigation
import Dict exposing (Dict)
import Dict.Fun
import Generated.Data as Image
    exposing
        ( CoordinateInfo
        , Expression
        , ExpressionResult(..)
        , Filename(..)
        , ImageCrop
        , Zones
        )
import Generated.Request as Request
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed
import Html.Lazy
import Icon
import Index exposing (Index)
import Input
import Json.Decode as Decode
import Json.Encode as Encode
import List.Zipper as Zipper exposing (Zipper)
import Process
import ProcessingState exposing (ProcessingState(..))
import Reorderable exposing (Reorderable)
import Route
import String.Interpolate exposing (interpolate)
import Task
import Url.Builder
import Util exposing (..)



-- PORT


port previewReady : (String -> msg) -> Sub msg



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions { clipboard, imageCropMode, filmRoll } =
    let
        current =
            Zipper.current filmRoll

        ifJust f =
            Maybe.withDefault Sub.none << Maybe.map (Browser.Events.onKeyDown << f)
    in
    Sub.batch
        [ Browser.Events.onKeyDown <|
            withCtrl <|
                Decode.oneOf
                    [ matchKey "s" SaveSettings
                    , matchKey "u" Undo
                    , matchKey "c" (CopySettings (Just current))
                    , matchKey "r" Rotate
                    , matchKey "h" PreviousImage
                    , matchKey "l" NextImage
                    , matchKey "3" (UpdateScale 0.3)
                    , matchKey "4" (UpdateScale 0.4)
                    , matchKey "5" (UpdateScale 0.5)
                    , matchKey "6" (UpdateScale 0.6)
                    , matchKey "7" (UpdateScale 0.7)
                    , matchKey "8" (UpdateScale 0.8)
                    , matchKey "0" (UpdateScale 1)
                    , matchKey "f" ToggleFullscreen
                    ]
        , ifJust
            (always <|
                matchKey "Escape" (UpdateImageCropMode Nothing)
            )
            imageCropMode
        , ifJust
            (always <|
                matchKey "Escape" (CopySettings Nothing)
            )
            clipboard
        , previewReady (OnPreviewReady << Filename)
        , Browser.Events.onResize (\_ _ -> OnBrowserResize)
        ]



-- MODEL


type alias Model =
    { checkExpressionsKey : Key { checkExpressionsKey : () }
    , clipboard : Maybe Image.Settings
    , coordinateInfo : Dict ( Float, Float ) CoordinateInfo
    , draftExpressions : DraftExpressions
    , filmRoll : Images
    , fullscreen : Bool
    , histogram : List Int
    , imageCropMode : Maybe ImageCrop
    , imageElement : Element
    , minimumRating : Int
    , notifications : List ( Level, String )
    , poster : Maybe Filename
    , previewColumns : Int
    , previewVersions : PreviewVersions
    , ratings : Ratings
    , route : Route.EditorRoute
    , scale : Float
    , undoState : UndoHistory
    , processingState : ProcessingState
    }


type alias UndoHistory =
    List Images


type alias Images =
    Zipper Image.Settings


type alias Ratings =
    Dict.Fun.Dict Filename String Int


type alias PreviewVersions =
    Dict.Fun.Dict Filename String Int


type alias DraftExpressions =
    Reorderable ( Maybe ExpressionResult, Expression )


type alias EditorIndex =
    Index { editor : () }


init : Route.EditorRoute -> Images -> Ratings -> Maybe Filename -> Model
init route filmRoll ratings poster =
    let
        focussed =
            focus route filmRoll
    in
    { processingState = ProcessingState.preview
    , ratings = ratings
    , poster = poster
    , filmRoll = focussed
    , draftExpressions = fromArray (.iExpressions (Zipper.current focussed))
    , checkExpressionsKey = Key 0
    , imageCropMode = Nothing
    , clipboard = Nothing
    , histogram = List.repeat 255 0
    , undoState = []
    , scale = 1
    , minimumRating = 0
    , previewColumns = 4
    , route = route
    , notifications = []
    , previewVersions = Dict.Fun.empty (\(Filename n) -> n) Filename
    , imageElement =
        { scene = { width = 0, height = 0 }
        , viewport = { x = 0, y = 0, width = 0, height = 0 }
        , element = { x = 0, y = 0, width = 0, height = 0 }
        }
    , coordinateInfo = Dict.empty
    , fullscreen = False
    }


continue : Route.EditorRoute -> Images -> Ratings -> Maybe Filename -> Model -> Model
continue route filmRoll ratings poster model =
    let
        focussed =
            focus route filmRoll
    in
    { model
        | processingState = ProcessingState.preview
        , ratings = ratings
        , poster = poster
        , draftExpressions = fromArray (.iExpressions (Zipper.current focussed))
        , filmRoll = focussed
        , route = route
        , coordinateInfo = Dict.empty
    }


focus : Route.EditorRoute -> Images -> Images
focus route filmRoll =
    Maybe.withDefault filmRoll <|
        Zipper.findFirst ((==) route.filename << .iFilename) filmRoll



-- UPDATE


type Msg
    = GotSaveImageSettings (HttpResult Image.FilmRoll)
    | GotGenerate String (HttpResult ())
    | GotHistogram (HttpResult (List Int))
    | RotatePreview Filename Float
    | OnImageSettingsChange Image.Settings
    | OnExpressionValueChange EditorIndex Expression
    | OnExpressionChange EditorIndex Expression
    | AddExpression
    | RemoveExpression EditorIndex
    | CheckExpressions (Key { checkExpressionsKey : () })
    | GotCheckExpressions (HttpResult (List ExpressionResult))
    | OnImageLoad String Image.Settings
    | SaveSettings
    | GenerateHighres
    | GenerateWallpaper
    | CopySettings (Maybe Image.Settings)
    | ApplyCopyToAll Images
    | UpdateImageCropMode (Maybe ImageCrop)
    | ApplyCrop ImageCrop
    | PreviousImage
    | NextImage
    | Undo
    | UpdateScale Float
    | SetColumnCount Int
    | Rate Filename Int
    | SetMinRating Int
    | Rotate
    | RemoveNotification
    | OnPreviewReady Filename
    | LoadOriginal
    | OnImageClick ( Float, Float )
    | RemoveCoordinate CoordinateInfo
    | RequestCoordinateInfo ( Float, Float ) Element
    | GotCoordinateInfo (HttpResult (List CoordinateInfo))
    | OnBrowserResize
    | GotElementPosition Element
    | OpenExternalEditor
    | GotOpenExternalEditor (HttpResult ())
    | ToggleFullscreen


update : Navigation.Key -> Msg -> Model -> ( Model, Cmd Msg )
update key msg model =
    case msg of
        GotHistogram result ->
            ( { model | histogram = Result.withDefault [] result }, Cmd.none )

        GotSaveImageSettings (Ok _) ->
            pushNotification Normal RemoveNotification "Saved settings" model

        GotSaveImageSettings (Err _) ->
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
            ( { model | filmRoll = filmRoll }
            , Cmd.none
            )

        OnImageSettingsChange settings ->
            ( updateSettings (\_ -> settings) model, Cmd.none )

        OnExpressionValueChange index val ->
            ( { model
                | checkExpressionsKey = nextKey model.checkExpressionsKey
                , draftExpressions =
                    Index.withIndex Reorderable.update
                        index
                        (Tuple.mapSecond (always val))
                        model.draftExpressions
              }
            , Task.perform (\_ -> CheckExpressions (nextKey model.checkExpressionsKey)) <|
                Process.sleep 1000
            )

        OnExpressionChange index val ->
            ( { model
                | checkExpressionsKey = nextKey model.checkExpressionsKey
                , draftExpressions =
                    Index.withIndex Reorderable.update
                        index
                        (Tuple.mapSecond (always val))
                        model.draftExpressions
              }
            , Task.perform (\_ -> CheckExpressions (nextKey model.checkExpressionsKey)) <|
                Process.sleep 1000
            )

        AddExpression ->
            ( { model
                | draftExpressions =
                    Reorderable.push ( Nothing, emptyExpression ) model.draftExpressions
              }
            , Cmd.none
            )

        RemoveExpression index ->
            let
                remove i arr =
                    Array.append
                        (Array.slice 0 i arr)
                        (Array.slice (i + 1) (Array.length arr) arr)
            in
            ( updateSettings
                (\s -> { s | iExpressions = Index.withIndex remove index s.iExpressions })
                { model
                    | draftExpressions = Index.withIndex Reorderable.drop index model.draftExpressions
                }
            , Cmd.none
            )

        CheckExpressions checkExpressionsKey ->
            if checkExpressionsKey /= model.checkExpressionsKey then
                ( model, Cmd.none )

            else
                ( model
                , Cmd.map GotCheckExpressions <|
                    Request.postImageSettingsExpressions <|
                        List.map Tuple.second <|
                            Reorderable.toList model.draftExpressions
                )

        GotCheckExpressions (Ok result) ->
            let
                isOk r =
                    case r of
                        SampleEval _ ->
                            True

                        SyntaxError _ ->
                            False

                        TypeError _ ->
                            False

                toArray =
                    Array.fromList << List.map Tuple.second << Reorderable.toList

                draftExpressions =
                    Tuple.second <|
                        List.foldl
                            (\x ( i, acc ) ->
                                ( i + 1, Reorderable.update i (Tuple.mapFirst (always (Just x))) acc )
                            )
                            ( 0, model.draftExpressions )
                            result
            in
            if List.all isOk result then
                ( updateSettings (\s -> { s | iExpressions = toArray model.draftExpressions })
                    { model | draftExpressions = draftExpressions }
                , Cmd.none
                )

            else
                ( { model | draftExpressions = draftExpressions }
                , Cmd.none
                )

        GotCheckExpressions (Err _) ->
            pushNotification Warning RemoveNotification "Unknown expression error" model

        CopySettings settings ->
            ( { model
                | clipboard = settings
                , imageCropMode = Nothing
              }
            , Cmd.none
            )

        ApplyCopyToAll filmRoll ->
            let
                currentChanged =
                    Zipper.current model.filmRoll /= Zipper.current filmRoll
            in
            if currentChanged then
                ( updateSettings identity { model | filmRoll = filmRoll }
                , Cmd.none
                )

            else
                ( { model | filmRoll = filmRoll }, Cmd.none )

        OnImageLoad dir settings ->
            let
                getHistogram =
                    Cmd.map GotHistogram <|
                        Request.postImageSettingsHistogram dir settings

                updateCoordinateInfo =
                    if Dict.isEmpty model.coordinateInfo then
                        Cmd.none

                    else
                        Cmd.map GotCoordinateInfo <|
                            Request.postImageSettingsCoordinate model.route.dir
                                ( Dict.keys model.coordinateInfo
                                , Zipper.current model.filmRoll
                                )
            in
            case model.processingState of
                Preview _ ->
                    ( model, Cmd.none )

                Ready _ ->
                    ( model, Cmd.none )

                Processing state ->
                    ( { model | processingState = ProcessingState.toReady state }
                    , Cmd.batch [ getHistogram, updateCoordinateInfo ]
                    )

                Queued state ->
                    ( { model
                        | processingState = ProcessingState.toProcessing state
                        , filmRoll = ProcessingState.toData state
                      }
                    , Cmd.none
                    )

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
                ("Preview ready: " ++ Image.filenameToString filename)
                { model | previewVersions = Dict.Fun.update filename f model.previewVersions }

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
            ( { model
                | imageCropMode = mode
                , clipboard = Nothing
                , processingState = fromPreview model.processingState
              }
            , Cmd.none
            )

        ApplyCrop imageCrop ->
            ( updateSettings (\s -> { s | iCrop = imageCrop })
                { model | imageCropMode = Nothing }
            , Cmd.none
            )

        PreviousImage ->
            let
                f x =
                    { dir = model.route.dir, filename = x.iFilename }
            in
            ( { model | undoState = [] }
            , Navigation.pushUrl key <|
                (Route.toUrl << Route.Editor << f << Zipper.current) <|
                    Maybe.withDefault (Zipper.last model.filmRoll) <|
                        Zipper.previous model.filmRoll
            )

        NextImage ->
            let
                f x =
                    { dir = model.route.dir, filename = x.iFilename }
            in
            ( { model | undoState = [] }
            , Navigation.pushUrl key <|
                (Route.toUrl << Route.Editor << f << Zipper.current) <|
                    Maybe.withDefault (Zipper.first model.filmRoll) <|
                        Zipper.next model.filmRoll
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
            ( { model | scale = val }
            , Task.attempt (GotElementPosition << Result.withDefault model.imageElement) <|
                Browser.Dom.getElement "image"
            )

        SetColumnCount scale ->
            ( { model | previewColumns = scale }, Cmd.none )

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
                    { model | ratings = Dict.Fun.update filename check model.ratings }
            in
            ( newModel
            , saveSettings newModel
            )

        SetMinRating rating ->
            ( { model | minimumRating = rating }, Cmd.none )

        RemoveNotification ->
            ( { model
                | notifications =
                    List.take (List.length model.notifications - 1)
                        model.notifications
              }
            , Cmd.none
            )

        LoadOriginal ->
            ( { model | processingState = fromPreview model.processingState }, Cmd.none )

        RemoveCoordinate { ciX, ciY } ->
            ( { model | coordinateInfo = Dict.remove ( ciX, ciY ) model.coordinateInfo }
            , Cmd.none
            )

        OnImageClick coordinate ->
            ( model
            , Task.attempt (RequestCoordinateInfo coordinate << Result.withDefault model.imageElement) <|
                Browser.Dom.getElement "image"
            )

        RequestCoordinateInfo ( x, y ) ({ element } as imageElement) ->
            let
                ( cX, cY ) =
                    ( (x - element.x) / element.width, (y - element.y) / element.height )
            in
            ( { model
                | imageElement = imageElement
                , processingState = fromPreview model.processingState
                , coordinateInfo =
                    Dict.insert ( cX, cY ) (CoordinateInfo cX cY 0) model.coordinateInfo
              }
            , Cmd.map GotCoordinateInfo <|
                Request.postImageSettingsCoordinate model.route.dir
                    ( [ ( cX, cY ) ], Zipper.current model.filmRoll )
            )

        GotCoordinateInfo (Ok coordinateInfo) ->
            ( { model
                | coordinateInfo =
                    List.foldl
                        (\c ->
                            Dict.insert ( c.ciX, c.ciY ) c
                        )
                        model.coordinateInfo
                        coordinateInfo
              }
            , Cmd.none
            )

        GotCoordinateInfo (Err _) ->
            ( model, Cmd.none )

        GotElementPosition element ->
            ( { model | imageElement = element }, Cmd.none )

        OnBrowserResize ->
            ( model
            , Task.attempt (GotElementPosition << Result.withDefault model.imageElement) <|
                Browser.Dom.getElement "image"
            )

        OpenExternalEditor ->
            ( model
            , Cmd.map GotOpenExternalEditor <|
                Request.postImageSettingsExternaleditor model.route.dir <|
                    Zipper.current model.filmRoll
            )

        GotOpenExternalEditor (Ok _) ->
            pushNotification Normal RemoveNotification "Opening external editor" model

        GotOpenExternalEditor (Err _) ->
            pushNotification Warning RemoveNotification "Error opening external editor" model

        ToggleFullscreen ->
            ( { model | fullscreen = not model.fullscreen }, Cmd.none )


type Key a
    = Key Int


nextKey : Key a -> Key a
nextKey (Key k) =
    Key (k + 1)


saveSettings : Model -> Cmd Msg
saveSettings model =
    Cmd.map GotSaveImageSettings <|
        Request.postImageSettings model.route.dir <|
            fromZipper model.poster model.ratings model.filmRoll


fromPreview : ProcessingState -> ProcessingState
fromPreview state =
    case state of
        Preview s ->
            ProcessingState.toProcessing s

        other ->
            other


updateSettings : (Image.Settings -> Image.Settings) -> Model -> Model
updateSettings f model =
    let
        unlessUnchanged old new =
            if old.filmRoll == new.filmRoll then
                old

            else
                new
    in
    case model.processingState of
        Preview state ->
            unlessUnchanged model
                { model
                    | processingState = ProcessingState.toProcessing state
                    , filmRoll = Zipper.mapCurrent f model.filmRoll
                    , undoState = model.filmRoll :: model.undoState
                }

        Ready state ->
            unlessUnchanged model
                { model
                    | processingState = ProcessingState.toProcessing state
                    , filmRoll = Zipper.mapCurrent f model.filmRoll
                    , undoState = model.filmRoll :: model.undoState
                }

        Processing state ->
            { model
                | processingState =
                    ProcessingState.toQueued
                        (Zipper.mapCurrent f model.filmRoll)
                        state
                , undoState = model.filmRoll :: model.undoState
            }

        Queued state ->
            { model
                | processingState = ProcessingState.map (Zipper.mapCurrent f) state
            }


fromZipper : Maybe Filename -> Ratings -> Images -> Image.FilmRoll
fromZipper poster ratings =
    Image.FilmRoll poster ratings
        << Dict.Fun.fromList Image.filenameToString Filename
        << List.map (\x -> ( x.iFilename, x ))
        << Zipper.toList



-- VIEW


view : Model -> List ( Level, String ) -> Html Msg
view model otherNotifications =
    main_ [ classList [ ( "fullscreen", model.fullscreen ) ] ]
        [ Html.Lazy.lazy viewNav model.route
        , Html.Lazy.lazy viewLoading model.processingState
        , Html.Lazy.lazy8 viewImage
            model.filmRoll
            model.route
            model.imageCropMode
            model.scale
            model.processingState
            model.previewVersions
            model.coordinateInfo
            model.imageElement
        , Html.Lazy.lazy5 viewSettingsLeft
            model.filmRoll
            model.undoState
            model.imageCropMode
            model.clipboard
            model.processingState
        , Html.Lazy.lazy4 viewSettingsRight
            model.filmRoll
            model.draftExpressions
            model.histogram
            model.processingState
        , Html.Lazy.lazy6 viewFiles
            model.route
            model.previewColumns
            model.minimumRating
            model.previewVersions
            model.ratings
            model.filmRoll
        , viewNotifications (model.notifications ++ otherNotifications)
        ]



-- NAV


viewNav : Route.EditorRoute -> Html Msg
viewNav route =
    nav []
        [ a [ href "/" ] [ text "browser" ]
        , text "/"
        , text route.dir
        , text "/"
        , text <|
            Image.filenameToString route.filename
        ]



-- FILES


viewFiles : Route.EditorRoute -> Int -> Int -> PreviewVersions -> Ratings -> Images -> Html Msg
viewFiles route columns minimumRating previewVersions ratings filmRoll =
    section [ class "files" ]
        [ Input.viewRangeInt 1 ( 2, 13, 5 ) "Columns" columns SetColumnCount
        , Input.viewRangeInt 1 ( 0, 5, 0 ) "Rating" minimumRating SetMinRating
        , Html.Keyed.ul [] <|
            List.map (\( _, Filename filename, x ) -> ( filename, x )) <|
                List.filter (\( rating, _, _ ) -> rating >= minimumRating) <|
                    List.concat
                        [ List.map (viewFilesLink False route.dir columns previewVersions ratings) <|
                            Zipper.before filmRoll
                        , [ viewFilesLink True route.dir columns previewVersions ratings <|
                                Zipper.current filmRoll
                          ]
                        , List.map (viewFilesLink False route.dir columns previewVersions ratings) <|
                            Zipper.after filmRoll
                        ]
        ]


viewFilesLink :
    Bool
    -> String
    -> Int
    -> PreviewVersions
    -> Ratings
    -> Image.Settings
    -> ( Int, Filename, Html Msg )
viewFilesLink isCurrent dir columns previewVersions ratings settings =
    let
        width =
            style "width" <|
                interpolate "calc({0}% - 1rem)" [ String.fromInt (100 // columns) ]

        rotate deg =
            fractionalModBy (degrees -360) (settings.iRotate - degrees deg)
    in
    ( Maybe.withDefault 0 (Dict.Fun.get settings.iFilename ratings)
    , settings.iFilename
    , li [ classList [ ( "-current", isCurrent ), ( "-small", columns > 4 ) ], width ]
        [ a [ href (Route.toUrl (Route.Editor { filename = settings.iFilename, dir = dir })) ]
            [ img
                [ src <|
                    Url.Builder.absolute
                        [ dir, "previews", previewExtension settings.iFilename ]
                        [ Url.Builder.int "v" <|
                            Maybe.withDefault 0 (Dict.Fun.get settings.iFilename previewVersions)
                        ]
                ]
                []
            ]
        , span [ class "files-file-rotate" ]
            [ button
                [ onClick (RotatePreview settings.iFilename (rotate 270))
                ]
                [ text "⊤" ]
            , button
                [ onClick (RotatePreview settings.iFilename (rotate 180))
                ]
                [ text "⊤" ]
            , button
                [ onClick (RotatePreview settings.iFilename (rotate 90))
                ]
                [ text "⊤" ]
            ]
        , span [ class "files-file-footer" ]
            [ text <|
                Image.filenameToString settings.iFilename
            , button [ onClick (CopySettings (Just settings)) ] [ Icon.copy ]
            , viewRating settings.iFilename ratings
            ]
        ]
    )


viewRating : Filename -> Ratings -> Html Msg
viewRating filename ratings =
    let
        rating =
            Maybe.withDefault 0 <|
                Dict.Fun.get filename ratings

        gliph n =
            if n <= rating then
                Icon.starred

            else
                Icon.unstarred
    in
    div [ class "ratings" ] <|
        List.map
            (\n -> button [ onClick (Rate filename n) ] [ gliph n ])
            (List.range 1 5)



-- IMAGE SETTINGS


viewSettingsRight :
    Images
    -> DraftExpressions
    -> List Int
    -> ProcessingState
    -> Html Msg
viewSettingsRight filmRoll draftExpressions histogram processingState =
    let
        ({ iZones } as settings) =
            settingsFromState processingState filmRoll

        zoneInput f value name =
            Input.viewRange 0.001 ( -0.25, 0.25, 0 ) name value <|
                \v -> { settings | iZones = f v }
    in
    section [ class "image-settings-right" ]
        [ viewSettingsGroup
            [ Html.Lazy.lazy viewHistogram histogram ]
        , viewSettingsGroup <|
            List.map (Html.map OnImageSettingsChange)
                [ Input.viewRange 0.1 ( 0, 10, 2.2 ) "Gamma" settings.iGamma <|
                    \v -> { settings | iGamma = v }
                , Input.viewRange 0.01 ( -0.75, 0.75, 0 ) "Blackpoint" settings.iBlackpoint <|
                    \v -> { settings | iBlackpoint = v }
                , Input.viewRange 0.01 ( 0.25, 1.75, 1 ) "Whitepoint" settings.iWhitepoint <|
                    \v -> { settings | iWhitepoint = v }
                , Input.viewRange 0.001 ( -0.25, 0.25, 0 ) "Pop" iZones.z7 <|
                    \v ->
                        { settings
                            | iZones =
                                { iZones | z2 = threeDecimalFloat (-v * 1.25), z7 = v }
                        }
                ]
        , viewSettingsGroup <|
            List.map (Html.map OnImageSettingsChange) <|
                [ zoneInput (\v -> { iZones | z1 = v }) iZones.z1 "I"
                , zoneInput (\v -> { iZones | z2 = v }) iZones.z2 "II"
                , zoneInput (\v -> { iZones | z3 = v }) iZones.z3 "III"
                , zoneInput (\v -> { iZones | z4 = v }) iZones.z4 "IV"
                , zoneInput (\v -> { iZones | z5 = v }) iZones.z5 "V"
                , zoneInput (\v -> { iZones | z6 = v }) iZones.z6 "VI"
                , zoneInput (\v -> { iZones | z7 = v }) iZones.z7 "VII"
                , zoneInput (\v -> { iZones | z8 = v }) iZones.z8 "VIII"
                , zoneInput (\v -> { iZones | z9 = v }) iZones.z9 "IX"
                ]
        , viewSettingsGroup
            [ Html.Keyed.node "div" [] <|
                Index.indexedMap
                    List.indexedMap
                    (Tuple.mapSecond << viewExpressionEditor draftExpressions)
                    (Reorderable.toKeyedList draftExpressions)
            ]
        ]


viewSettingsLeft :
    Images
    -> UndoHistory
    -> Maybe ImageCrop
    -> Maybe Image.Settings
    -> ProcessingState
    -> Html Msg
viewSettingsLeft filmRoll undoState imageCropMode clipboard_ processingState =
    let
        settings =
            settingsFromState processingState filmRoll

        clipboardTitle x clipboard =
            interpolate "{0} from {1}" [ x, Image.filenameToString clipboard.iFilename ]
    in
    div [ class "image-settings-left" ]
        [ viewSettingsGroup
            [ viewImageCropMode settings imageCropMode
            , button [ onClick Rotate, title "rotate ccw" ] [ Icon.rotate ]
            , button [ onClick AddExpression, title "add expression" ]
                [ Icon.lambda ]
            ]
        , viewSettingsGroup
            [ button
                [ onClick (CopySettings (Just settings))
                , title "copy settings"
                ]
                [ Icon.copy ]
            , viewMaybe clipboard_ <|
                \clipboard ->
                    div [ class "image-settings-paste" ]
                        [ viewClipboardButton (clipboardTitle "bot" clipboard)
                            Icon.applyBoth
                            { clipboard | iFilename = settings.iFilename }
                        , viewClipboardButton (clipboardTitle "tone" clipboard)
                            Icon.applyTone
                            { clipboard
                                | iFilename = settings.iFilename
                                , iRotate = settings.iRotate
                                , iCrop = settings.iCrop
                            }
                        , viewClipboardButton (clipboardTitle "crop" clipboard)
                            Icon.applyCrop
                            { settings | iCrop = clipboard.iCrop }
                        , button
                            [ onClick <|
                                ApplyCopyToAll <|
                                    Zipper.map (\i -> { clipboard | iFilename = i.iFilename }) filmRoll
                            , title (clipboardTitle "apply to all" clipboard)
                            ]
                            [ Icon.applyAll ]
                        , button
                            [ onClick <|
                                ApplyCopyToAll <|
                                    Zipper.map
                                        (\i ->
                                            { clipboard
                                                | iFilename = i.iFilename
                                                , iRotate = i.iRotate
                                                , iCrop = i.iCrop
                                            }
                                        )
                                        filmRoll
                            , title (clipboardTitle "apply tone to all" clipboard)
                            ]
                            [ Icon.applyAllTone ]
                        , button
                            [ onClick <|
                                ApplyCopyToAll <|
                                    Zipper.map (\i -> { i | iCrop = clipboard.iCrop }) filmRoll
                            , title (clipboardTitle "apply crop to all" clipboard)
                            ]
                            [ Icon.applyAllCrop ]
                        , button
                            [ onClick <|
                                ApplyCopyToAll <|
                                    Zipper.map (\i -> { i | iRotate = clipboard.iRotate }) filmRoll
                            , title (clipboardTitle "apply rotate to all" clipboard)
                            ]
                            [ Icon.applyAllRotate ]
                        ]
            ]
        , viewSettingsGroup
            [ button
                [ onClick (OnImageSettingsChange (resetAll settings))
                , title "reset"
                ]
                [ Icon.reset ]
            , button
                [ onClick (OnImageSettingsChange (resetTone settings))
                , title "reset tone"
                ]
                [ Icon.resetTone ]
            , button
                [ disabled (List.isEmpty undoState)
                , onClick Undo
                , title "undo"
                ]
                [ Icon.undo ]
            ]
        , viewSettingsGroup
            [ button [ onClick SaveSettings, title "save" ] [ Icon.save ]
            , button [ onClick GenerateHighres, title "generate highres" ]
                [ Icon.highres ]
            , button [ onClick GenerateWallpaper, title "generate wallpaper" ]
                [ Icon.wallpaper ]
            , button [ onClick OpenExternalEditor, title "open external" ]
                [ Icon.externalEditor ]
            ]
        , viewSettingsGroup
            [ button [ onClick NextImage ] [ Icon.right ]
            , button [ onClick PreviousImage ] [ Icon.left ]
            ]
        , viewIf (processingState == ProcessingState.preview) <|
            \_ ->
                viewSettingsGroup
                    [ button [ onClick LoadOriginal, title "load original" ]
                        [ Icon.original ]
                    ]
        ]


viewExpressionEditor :
    DraftExpressions
    -> EditorIndex
    -> ( Maybe ExpressionResult, Expression )
    -> Html Msg
viewExpressionEditor draftExpressions index ( expressionResult, expression ) =
    let
        onRangeInput v =
            OnExpressionValueChange index { expression | eValue = v }

        onTextInput v =
            OnExpressionChange index { expression | eExpr = v }
    in
    div [ class "expression-editor" ]
        [ span [ class "expression-editor-hint" ]
            [ text "λ"
            , button
                [ onClick (RemoveExpression index)
                , class "expression-editor-remove"
                , title "remove"
                ]
                []
            ]
        , textarea
            [ onInput onTextInput
            , spellcheck False
            , autocomplete False
            , value expression.eExpr
            , rows (List.length (String.lines expression.eExpr))
            ]
            []
        , viewMaybe (Index.withIndex Reorderable.get index draftExpressions) <|
            \( _, { eValue } ) -> Input.viewRange 0.01 ( -1, 1, 0 ) "n" eValue onRangeInput
        , viewMaybe expressionResult <|
            \result ->
                case result of
                    SampleEval xs ->
                        viewSampleEval xs

                    SyntaxError err ->
                        pre []
                            [ text "Syntax error\n", text err ]

                    TypeError err ->
                        pre []
                            [ text "Type error\n", text err ]
        ]


viewSampleEval : List Float -> Html Msg
viewSampleEval offset =
    div [ class "expression-editor-sample" ] <|
        List.map
            (\v ->
                span [ class "expression-editor-sample-part" ]
                    [ span
                        [ style "height" <|
                            interpolate "{0}rem"
                                [ String.fromFloat (abs (v * 2)) ]
                        , class "expression-editor-sample-bar"
                        ]
                        [ span
                            [ class "expression-editor-sample-value"
                            , style "transform" <|
                                interpolate "translateY(calc({0}rem + .25rem)) rotate(90deg)"
                                    [ String.fromFloat (abs (v * 2)) ]
                            ]
                            [ text (String.fromFloat (threeDecimalFloat v)) ]
                        ]
                    ]
            )
            offset


viewSettingsGroup : List (Html Msg) -> Html Msg
viewSettingsGroup =
    div [ class "image-settings-group" ]


viewClipboardButton : String -> Html Msg -> Image.Settings -> Html Msg
viewClipboardButton desc icon settings =
    button [ onClick (OnImageSettingsChange settings), title desc ] [ icon ]


viewImageCropMode : Image.Settings -> Maybe ImageCrop -> Html Msg
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
            button [ onClick (UpdateImageCropMode (Just current.iCrop)), title "crop" ] [ Icon.crop ]

        Just imageCrop ->
            div []
                [ button [ onClick (UpdateImageCropMode Nothing) ] [ Icon.crop ]
                , div [ class "crop-settings" ]
                    [ Input.viewRange 0.01 ( 0, 5, 0 ) "Top" imageCrop.icTop <|
                        onTopChange imageCrop
                    , Input.viewRange 0.01 ( 0, 5, 0 ) "Left" imageCrop.icLeft <|
                        onLeftChange imageCrop
                    , Input.viewRange 0.1 ( 85, 100, 100 ) "Width" imageCrop.icWidth <|
                        onWidthChange imageCrop
                    , button [ onClick (UpdateImageCropMode Nothing) ] [ Icon.cancel ]
                    , button [ onClick (ApplyCrop imageCrop) ] [ Icon.ok ]
                    ]
                ]



-- IMAGE


viewImage :
    Images
    -> Route.EditorRoute
    -> Maybe ImageCrop
    -> Float
    -> ProcessingState
    -> PreviewVersions
    -> Dict ( Float, Float ) CoordinateInfo
    -> Element
    -> Html Msg
viewImage filmRoll route imageCropMode scale_ processingState previewVersions coordinateInfo element =
    let
        current =
            Zipper.current filmRoll

        ifCropping settings =
            Maybe.withDefault settings <|
                Maybe.map (\_ -> { settings | iRotate = 0, iCrop = ImageCrop 0 0 100 })
                    imageCropMode

        scale =
            case imageCropMode of
                Just _ ->
                    style "transform" "scale(1)"

                Nothing ->
                    style "transform" <|
                        interpolate "scale({0})" [ String.fromFloat scale_ ]
    in
    section [ id "image-section" ]
        [ div [ class "image-wrapper" ]
            [ case processingState of
                Preview _ ->
                    img
                        [ style "user-select" "none"
                        , onCoordinateClick
                        , id "image"
                        , scale
                        , src <|
                            Url.Builder.absolute
                                [ route.dir, "previews", previewExtension current.iFilename ]
                                [ Url.Builder.int "v" <|
                                    Maybe.withDefault 0 <|
                                        Dict.Fun.get current.iFilename previewVersions
                                ]
                        ]
                        []

                _ ->
                    img
                        [ on "load" (Decode.succeed (OnImageLoad route.dir current))
                        , onCoordinateClick
                        , id "image"
                        , style "user-select" "none"
                        , scale
                        , src <|
                            Url.Builder.absolute [ "image" ]
                                [ toImageUrlParams (ifCropping current)
                                , Url.Builder.string "dir" route.dir
                                ]
                        ]
                        []
            , Html.Keyed.node "div" [ class "coordinate-info" ] <|
                List.map (viewCoordinate element current scale_) <|
                    Dict.values coordinateInfo
            , viewMaybe imageCropMode <|
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
        , section [ class "zoom" ]
            [ Input.viewRange 0.01 ( 0.05, 1.05, 1 ) "Zoom" scale_ UpdateScale
            ]
        ]


viewCoordinate : Element -> Image.Settings -> Float -> CoordinateInfo -> ( String, Html Msg )
viewCoordinate { element } settings scale coordinate =
    let
        xOffset =
            (element.width / scale - element.width) / 2

        yOffset =
            (element.height / scale - element.height) / 2
    in
    ( String.fromFloat coordinate.ciX ++ String.fromFloat coordinate.ciY
    , span
        [ style "top" <|
            interpolate "{0}px"
                [ String.fromFloat (element.height * coordinate.ciY + yOffset) ]
        , style "left" <|
            interpolate "{0}px"
                [ String.fromFloat (element.width * coordinate.ciX + xOffset) ]
        , classList
            [ ( "-dark", coordinate.ciValue > 0.5 )
            , ( "-flip", coordinate.ciX > 0.85 )
            , ( "-flap", coordinate.ciY > 0.95 )
            , ( "coordinate", True )
            ]
        ]
        [ button [ onClick (RemoveCoordinate coordinate) ]
            [ text (String.left 7 (String.fromFloat coordinate.ciValue)) ]
        , span [ class "coordinate-buttons" ]
            [ button
                [ class "coordinate-min"
                , onClick <|
                    OnImageSettingsChange <|
                        updateZoneByInt
                            (round (coordinate.ciValue * 10))
                            (\v -> v - 0.025)
                            settings
                ]
                [ text "-" ]
            , button
                [ class "coordinate-plus"
                , onClick <|
                    OnImageSettingsChange <|
                        updateZoneByInt
                            (round (coordinate.ciValue * 10))
                            (\v -> v + 0.025)
                            settings
                ]
                [ text "+" ]
            ]
        ]
    )


onCoordinateClick : Attribute Msg
onCoordinateClick =
    on "click" <|
        Decode.map2 (\x y -> OnImageClick ( x, y ))
            (Decode.field "x" Decode.float)
            (Decode.field "y" Decode.float)


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


viewLoading : ProcessingState -> Html msg
viewLoading state =
    case state of
        Ready _ ->
            text ""

        Preview _ ->
            text ""

        Processing _ ->
            div [ class "loading-spinner" ] []

        Queued _ ->
            div [ class "loading-spinner" ] []



-- HELPERS


updateZoneByInt : Int -> (Float -> Float) -> Image.Settings -> Image.Settings
updateZoneByInt n f settings =
    let
        g =
            threeDecimalFloat << f

        set zones =
            case n of
                0 ->
                    { zones | z1 = g zones.z1 }

                1 ->
                    { zones | z1 = g zones.z1 }

                2 ->
                    { zones | z2 = g zones.z2 }

                3 ->
                    { zones | z3 = g zones.z3 }

                4 ->
                    { zones | z4 = g zones.z4 }

                5 ->
                    { zones | z5 = g zones.z5 }

                6 ->
                    { zones | z6 = g zones.z6 }

                7 ->
                    { zones | z7 = g zones.z7 }

                8 ->
                    { zones | z8 = g zones.z8 }

                9 ->
                    { zones | z9 = g zones.z9 }

                _ ->
                    { zones | z9 = g zones.z9 }
    in
    { settings | iZones = set settings.iZones }


threeDecimalFloat : Float -> Float
threeDecimalFloat x =
    toFloat (round (x * 1000)) / 1000


previewExtension : Filename -> String
previewExtension (Filename x) =
    String.dropRight 3 x ++ "jpg"


resetAll : Image.Settings -> Image.Settings
resetAll current =
    Image.Settings current.iFilename
        0
        (ImageCrop 0 0 100)
        2.2
        (Zones 0 0 0 0 0 0 0 0 0)
        0
        1
        Array.empty


resetTone : Image.Settings -> Image.Settings
resetTone current =
    Image.Settings current.iFilename
        current.iRotate
        current.iCrop
        2.2
        (Zones 0 0 0 0 0 0 0 0 0)
        0
        1
        Array.empty


emptyExpression : Expression
emptyExpression =
    Expression 0 -1 1 "" ""


toImageUrlParams : Image.Settings -> Url.Builder.QueryParameter
toImageUrlParams =
    Url.Builder.string "image-settings"
        << Base64.encode
        << Encode.encode 0
        << Image.encodeSettings


fractionalModBy : Float -> Float -> Float
fractionalModBy m v =
    v - m * Basics.toFloat (Basics.floor (v / m))


settingsFromState : ProcessingState -> Images -> Image.Settings
settingsFromState processingState filmRoll =
    case processingState of
        Queued queuedFilmRoll ->
            Zipper.current (ProcessingState.toData queuedFilmRoll)

        _ ->
            Zipper.current filmRoll


fromArray : Array a -> Reorderable ( Maybe b, a )
fromArray =
    Array.foldl Reorderable.push Reorderable.empty
        << Array.map (Tuple.pair Nothing)
