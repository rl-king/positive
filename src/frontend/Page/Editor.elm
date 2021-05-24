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
import Data.Id as Id exposing (CollectionId, ImageSettingsId)
import Data.Path as Path exposing (Filename)
import Dict exposing (Dict)
import Dict.Fun
import Generated.Data as Image
    exposing
        ( Collection
        , CoordinateInfo
        , Expression
        , ExpressionResult(..)
        , FilmRoll
        , ImageCrop
        , ImageSettings
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


port previewReady : (Int -> msg) -> Sub msg



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions { clipboard, imageCropMode, images } =
    let
        current =
            Zipper.current images

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
        , previewReady (OnPreviewReady << Id.fromInt)
        , Browser.Events.onResize (\_ _ -> OnBrowserResize)
        ]



-- MODEL


type alias Model =
    { filmRoll : FilmRoll
    , checkExpressionsKey : Key { checkExpressionsKey : () }
    , clipboard : Maybe ImageSettings
    , coordinateInfo : Dict ( Float, Float ) CoordinateInfo
    , draftExpressions : DraftExpressions
    , images : Images
    , fullscreen : Bool
    , imageCropMode : Maybe ImageCrop
    , imageElement : Element
    , minimumRating : Int
    , notifications : Notifications
    , previewColumns : Int
    , previewVersions : PreviewVersions
    , scale : Float
    , undoState : UndoHistory
    , processingState : ProcessingState
    , histogram : Maybe Histogram
    , collections : List Collection
    }


type alias UndoHistory =
    List Images


type alias Histogram =
    Array Int


type alias Images =
    Zipper ImageSettings


type alias PreviewVersions =
    Dict.Fun.Dict ImageSettingsId Int Int


type alias DraftExpressions =
    Reorderable ( Maybe ExpressionResult, Expression )


type alias EditorIndex =
    Index { editor : () }


init : FilmRoll -> List Collection -> ImageSettingsId -> Images -> Model
init filmRoll collections imageSettingsId images =
    let
        focussed =
            focus imageSettingsId images
    in
    { filmRoll = filmRoll
    , processingState = ProcessingState.preview
    , images = focussed
    , draftExpressions = fromArray (.expressions (Zipper.current focussed))
    , checkExpressionsKey = Key 0
    , imageCropMode = Nothing
    , clipboard = Nothing
    , undoState = []
    , scale = 1
    , minimumRating = 0
    , previewColumns = 4
    , notifications = emptyNotifications
    , previewVersions = Dict.Fun.empty Id.toInt Id.fromInt
    , imageElement =
        { scene = { width = 0, height = 0 }
        , viewport = { x = 0, y = 0, width = 0, height = 0 }
        , element = { x = 0, y = 0, width = 0, height = 0 }
        }
    , coordinateInfo = Dict.empty
    , fullscreen = False
    , histogram = Nothing
    , collections = collections
    }


continue : ImageSettingsId -> Images -> Model -> Model
continue imageSettingsId images model =
    let
        focussed =
            focus imageSettingsId images
    in
    { model
        | processingState = ProcessingState.preview
        , draftExpressions = fromArray (.expressions (Zipper.current focussed))
        , images = focussed
        , coordinateInfo = Dict.empty
        , histogram = Nothing
    }


focus : ImageSettingsId -> Images -> Images
focus imageSettingsId images =
    Maybe.withDefault images <|
        Zipper.findFirst ((==) imageSettingsId << .id) images



-- UPDATE


type Msg
    = GotSaveImageSettings (HttpResult Image.FilmRoll)
    | GotGenerate String (HttpResult ())
    | GotHistogram (HttpResult (Array Int))
    | GotToggleCollection (HttpResult (List Collection))
    | RotatePreview ImageSettingsId Float
    | OnImageSettingsChange ImageSettings
    | OnExpressionValueChange EditorIndex Expression
    | OnExpressionChange EditorIndex Expression
    | AddExpression
    | RemoveExpression EditorIndex
    | CheckExpressions (Key { checkExpressionsKey : () })
    | GotCheckExpressions (HttpResult (List ExpressionResult))
    | OnImageLoad ImageSettings
    | SaveSettings
    | GenerateHighres
    | GenerateWallpaper
    | CopySettings (Maybe ImageSettings)
    | ApplyCopyToAll Images
    | UpdateImageCropMode (Maybe ImageCrop)
    | ApplyCrop ImageCrop
    | PreviousImage
    | NextImage
    | Undo
    | UpdateScale Float
    | SetColumnCount Int
    | Rate ImageSettingsId Int
    | SetMinRating Int
    | Rotate
    | RemoveNotification NotificationId
    | OnPreviewReady ImageSettingsId
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
    | AddToCollection CollectionId ImageSettingsId
    | RemoveFromCollection CollectionId ImageSettingsId


update : Navigation.Key -> Msg -> Model -> ( Model, Cmd Msg )
update key msg model =
    case msg of
        GotHistogram result ->
            ( { model | histogram = Result.toMaybe result }
            , Cmd.none
            )

        GotSaveImageSettings (Ok _) ->
            pushNotification Normal RemoveNotification "Saved settings" model

        GotSaveImageSettings (Err _) ->
            pushNotification Warning RemoveNotification "Error saving settings" model

        GotGenerate type_ (Ok _) ->
            pushNotification Normal RemoveNotification ("Generated " ++ type_) model

        GotGenerate type_ (Err _) ->
            pushNotification Warning RemoveNotification ("Error generating " ++ type_) model

        GotToggleCollection (Ok collections) ->
            pushNotification Normal
                RemoveNotification
                "Updated collection"
                { model | collections = collections }

        GotToggleCollection (Err _) ->
            pushNotification Warning RemoveNotification "Error updating collection" model

        Rotate ->
            ( updateSettings
                (\s ->
                    { s
                        | rotate = fractionalModBy (degrees -360) (s.rotate - degrees 90)
                    }
                )
                model
            , Cmd.none
            )

        RotatePreview imageSettingsId rotation ->
            let
                rotate imageSettings =
                    { imageSettings | rotate = rotation }
            in
            ( { model | images = updateZipperById imageSettingsId rotate model.images }
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
                (\s -> { s | expressions = Index.withIndex remove index s.expressions })
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
                ( updateSettings (\s -> { s | expressions = toArray model.draftExpressions })
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

        ApplyCopyToAll images ->
            let
                currentChanged =
                    Zipper.current model.images /= Zipper.current images
            in
            if currentChanged then
                ( updateSettings identity { model | images = images }
                , Cmd.none
                )

            else
                ( { model | images = images }, Cmd.none )

        OnImageLoad settings ->
            let
                getHistogram =
                    Cmd.map GotHistogram <|
                        Request.postImageSettingsHistogram settings

                updateCoordinateInfo =
                    if Dict.isEmpty model.coordinateInfo then
                        Cmd.none

                    else
                        Cmd.map GotCoordinateInfo <|
                            Request.postImageSettingsCoordinate
                                ( Dict.keys model.coordinateInfo
                                , Zipper.current model.images
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
                        , images = ProcessingState.toData state
                      }
                    , Cmd.none
                    )

        OnPreviewReady imageSettingsId ->
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
                ("Preview ready: " ++ Id.toString imageSettingsId)
                { model | previewVersions = Dict.Fun.update imageSettingsId f model.previewVersions }

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
                                Request.postImageSettingsHighres <|
                                    Zipper.current model.images
                            , cmds
                            ]
                    )

        GenerateWallpaper ->
            pushNotification Normal RemoveNotification "Generating wallpaper version" model
                |> Tuple.mapSecond
                    (\cmds ->
                        Cmd.batch
                            [ Cmd.map (GotGenerate "wallpaper") <|
                                Request.postImageSettingsWallpaper <|
                                    Zipper.current model.images
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
            ( updateSettings (\s -> { s | crop = imageCrop })
                { model | imageCropMode = Nothing }
            , Cmd.none
            )

        PreviousImage ->
            ( { model | undoState = [] }
            , Navigation.pushUrl key <|
                (Route.toUrl << Route.Editor model.filmRoll.id << .id << Zipper.current) <|
                    Maybe.withDefault (Zipper.last model.images) <|
                        Zipper.previous model.images
            )

        NextImage ->
            ( { model | undoState = [] }
            , Navigation.pushUrl key <|
                (Route.toUrl << Route.Editor model.filmRoll.id << .id << Zipper.current) <|
                    Maybe.withDefault (Zipper.first model.images) <|
                        Zipper.next model.images
            )

        Undo ->
            case model.undoState of
                [] ->
                    ( model, Cmd.none )

                x :: xs ->
                    ( { model | undoState = xs, images = x }
                    , Cmd.none
                    )

        UpdateScale val ->
            ( { model | scale = val }
            , Task.attempt (GotElementPosition << Result.withDefault model.imageElement) <|
                Browser.Dom.getElement "image"
            )

        SetColumnCount scale ->
            ( { model | previewColumns = scale }, Cmd.none )

        Rate imageSettingsId rating ->
            let
                rate imageSettings =
                    { imageSettings | rating = rating }

                newModel =
                    { model
                        | images =
                            updateZipperById imageSettingsId rate model.images
                    }
            in
            ( newModel
            , saveSettings newModel
            )

        SetMinRating rating ->
            ( { model | minimumRating = rating }, Cmd.none )

        RemoveNotification notificationId ->
            ( { model
                | notifications =
                    removeNotification notificationId
                        model.notifications
              }
            , Cmd.none
            )

        LoadOriginal ->
            ( { model | processingState = fromPreview model.processingState }
            , Cmd.none
            )

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
                Request.postImageSettingsCoordinate
                    ( [ ( cX, cY ) ], Zipper.current model.images )
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
                Request.postImageSettingsExternaleditor <|
                    Zipper.current model.images
            )

        GotOpenExternalEditor (Ok _) ->
            pushNotification Normal RemoveNotification "Opening external editor" model

        GotOpenExternalEditor (Err _) ->
            pushNotification Warning RemoveNotification "Error opening external editor" model

        ToggleFullscreen ->
            ( { model | fullscreen = not model.fullscreen }, Cmd.none )

        AddToCollection collectionId imageSettingsId ->
            ( model
            , Cmd.map GotToggleCollection <|
                Request.postCollectionByCollectionIdByImageSettingsId
                    collectionId
                    imageSettingsId
            )

        RemoveFromCollection collectionId imageSettingsId ->
            ( model
            , Cmd.map GotToggleCollection <|
                Request.deleteCollectionByCollectionIdByImageSettingsId
                    collectionId
                    imageSettingsId
            )


type Key a
    = Key Int


nextKey : Key a -> Key a
nextKey (Key k) =
    Key (k + 1)


saveSettings : Model -> Cmd Msg
saveSettings { filmRoll, images } =
    Cmd.map GotSaveImageSettings <|
        Request.postFilmroll <|
            { filmRoll | imageSettings = Zipper.toList images }


fromPreview : ProcessingState -> ProcessingState
fromPreview state =
    case state of
        Preview s ->
            ProcessingState.toProcessing s

        other ->
            other


updateSettings : (ImageSettings -> ImageSettings) -> Model -> Model
updateSettings f model =
    let
        unlessUnchanged old new =
            if old.images == new.images then
                old

            else
                new
    in
    case model.processingState of
        Preview state ->
            unlessUnchanged model
                { model
                    | processingState = ProcessingState.toProcessing state
                    , images = Zipper.mapCurrent f model.images
                    , undoState = model.images :: model.undoState
                }

        Ready state ->
            unlessUnchanged model
                { model
                    | processingState = ProcessingState.toProcessing state
                    , images = Zipper.mapCurrent f model.images
                    , undoState = model.images :: model.undoState
                }

        Processing state ->
            { model
                | processingState =
                    ProcessingState.toQueued
                        (Zipper.mapCurrent f model.images)
                        state
                , undoState = model.images :: model.undoState
            }

        Queued state ->
            { model
                | processingState = ProcessingState.map (Zipper.mapCurrent f) state
            }



-- VIEW


view : Model -> Notifications -> Html Msg
view model otherNotifications =
    main_ [ classList [ ( "fullscreen", model.fullscreen ) ] ]
        [ Html.Lazy.lazy2 viewNav model.filmRoll model.images
        , Html.Lazy.lazy viewLoading model.processingState
        , Html.Lazy.lazy8 viewImage
            model.images
            model.filmRoll
            model.imageCropMode
            model.scale
            model.processingState
            model.previewVersions
            model.coordinateInfo
            model.imageElement
        , viewSettingsLeft
            model.images
            model.undoState
            model.imageCropMode
            model.clipboard
            model.processingState
        , viewSettingsRight
            model.images
            model.collections
            model.histogram
            model.draftExpressions
            model.processingState
        , Html.Lazy.lazy5 viewFiles
            model.filmRoll
            model.previewColumns
            model.minimumRating
            model.previewVersions
            model.images
        , viewNotifications (appendNotifications model.notifications otherNotifications)
        ]



-- NAV


viewNav : FilmRoll -> Images -> Html Msg
viewNav filmRoll images =
    nav []
        [ a [ href "/" ] [ text "browser" ]
        , text <|
            Path.toString filmRoll.directoryPath
        , text "/"
        , text <|
            (Path.toString << .filename) <|
                Zipper.current images
        ]



-- FILES


viewFiles : FilmRoll -> Int -> Int -> PreviewVersions -> Images -> Html Msg
viewFiles filmRoll columns minimumRating previewVersions images =
    section [ class "files" ]
        [ Input.viewRangeInt 1 ( 2, 13, 5 ) "Columns" columns SetColumnCount
        , Input.viewRangeInt 1 ( 0, 5, 0 ) "Rating" minimumRating SetMinRating
        , Html.Keyed.ul [] <|
            List.map (\( _, imageSettingsId, x ) -> ( Id.toString imageSettingsId, x )) <|
                List.filter (\( rating, _, _ ) -> rating >= minimumRating) <|
                    List.concat
                        [ List.map (viewFilesLink False filmRoll columns previewVersions) <|
                            Zipper.before images
                        , [ viewFilesLink True filmRoll columns previewVersions <|
                                Zipper.current images
                          ]
                        , List.map (viewFilesLink False filmRoll columns previewVersions) <|
                            Zipper.after images
                        ]
        ]


viewFilesLink :
    Bool
    -> FilmRoll
    -> Int
    -> PreviewVersions
    -> ImageSettings
    -> ( Int, ImageSettingsId, Html Msg )
viewFilesLink isCurrent filmRoll columns previewVersions settings =
    let
        width =
            style "width" <|
                interpolate "calc({0}% - 1rem)" [ String.fromInt (100 // columns) ]

        rotate deg =
            fractionalModBy (degrees -360) (settings.rotate - degrees deg)
    in
    ( settings.rating
    , settings.id
    , li [ classList [ ( "-current", isCurrent ), ( "-small", columns > 4 ) ], width ]
        [ a [ href (Route.toUrl (Route.Editor filmRoll.id settings.id)) ]
            [ img
                [ src <|
                    Url.Builder.crossOrigin (Path.toString filmRoll.directoryPath)
                        [ "previews", previewExtension settings.filename ]
                        [ Url.Builder.int "v" <|
                            Maybe.withDefault 0 (Dict.Fun.get settings.id previewVersions)
                        ]
                ]
                []
            ]
        , span [ class "files-file-rotate" ]
            [ button
                [ onClick (RotatePreview settings.id (rotate 270))
                ]
                [ text "⊤" ]
            , button
                [ onClick (RotatePreview settings.id (rotate 180))
                ]
                [ text "⊤" ]
            , button
                [ onClick (RotatePreview settings.id (rotate 90))
                ]
                [ text "⊤" ]
            ]
        , span [ class "files-file-footer" ]
            [ text <|
                Path.toString settings.filename
            , button [ onClick (CopySettings (Just settings)) ] [ Icon.copy ]
            , viewRating settings
            ]
        ]
    )


viewRating : ImageSettings -> Html Msg
viewRating settings =
    let
        gliph n =
            if n <= settings.rating then
                Icon.starred

            else
                Icon.unstarred
    in
    div [ class "ratings" ] <|
        List.map
            (\n -> button [ onClick (Rate settings.id n) ] [ gliph n ])
            (List.range 1 5)



-- IMAGE SETTINGS


viewSettingsRight :
    Images
    -> List Collection
    -> Maybe Histogram
    -> DraftExpressions
    -> ProcessingState
    -> Html Msg
viewSettingsRight images collections maybeHistogram draftExpressions processingState =
    let
        ({ zones } as settings) =
            settingsFromState processingState images

        zoneInput f value name =
            Input.viewRange 0.001 ( -0.25, 0.25, 0 ) name value <|
                \v -> { settings | zones = f v }
    in
    section [ class "image-settings-right" ]
        [ viewSettingsGroup
            [ Html.Lazy.lazy viewHistogram <|
                Maybe.withDefault (.histogram (Zipper.current images))
                    maybeHistogram
            ]
        , viewSettingsGroup <|
            List.map (Html.map OnImageSettingsChange)
                [ Input.viewRange 0.1 ( 0, 10, 2.2 ) "Gamma" settings.gamma <|
                    \v -> { settings | gamma = v }
                , Input.viewRange 0.01 ( -0.75, 0.75, 0 ) "Blackpoint" settings.blackpoint <|
                    \v -> { settings | blackpoint = v }
                , Input.viewRange 0.01 ( 0.25, 1.75, 1 ) "Whitepoint" settings.whitepoint <|
                    \v -> { settings | whitepoint = v }
                , Input.viewRange 0.001 ( -0.25, 0.25, 0 ) "Pop" zones.z7 <|
                    \v ->
                        { settings
                            | zones =
                                { zones | z2 = threeDecimalFloat (-v * 1.25), z7 = v }
                        }
                ]
        , viewSettingsGroup <|
            List.map (Html.map OnImageSettingsChange) <|
                [ zoneInput (\v -> { zones | z1 = v }) zones.z1 "I"
                , zoneInput (\v -> { zones | z2 = v }) zones.z2 "II"
                , zoneInput (\v -> { zones | z3 = v }) zones.z3 "III"
                , zoneInput (\v -> { zones | z4 = v }) zones.z4 "IV"
                , zoneInput (\v -> { zones | z5 = v }) zones.z5 "V"
                , zoneInput (\v -> { zones | z6 = v }) zones.z6 "VI"
                , zoneInput (\v -> { zones | z7 = v }) zones.z7 "VII"
                , zoneInput (\v -> { zones | z8 = v }) zones.z8 "VIII"
                , zoneInput (\v -> { zones | z9 = v }) zones.z9 "IX"
                ]
        , viewSettingsGroup
            [ Html.Keyed.node "div" [] <|
                Index.indexedMap
                    List.indexedMap
                    (Tuple.mapSecond << viewExpressionEditor draftExpressions)
                    (Reorderable.toKeyedList draftExpressions)
            ]
        , viewSettingsGroup <|
            [ div [ class "image-settings-collections" ] <|
                List.map (viewToggleCollection (Zipper.current images)) collections
            ]
        ]


viewToggleCollection : ImageSettings -> Collection -> Html Msg
viewToggleCollection { id } collection =
    let
        ( isMember, msg ) =
            if List.member id collection.imageIds then
                ( True, RemoveFromCollection collection.id id )

            else
                ( False, AddToCollection collection.id id )
    in
    button
        [ classList [ ( "-selected", isMember ) ]
        , onClick msg
        ]
        [ text collection.title ]


viewSettingsLeft :
    Images
    -> UndoHistory
    -> Maybe ImageCrop
    -> Maybe ImageSettings
    -> ProcessingState
    -> Html Msg
viewSettingsLeft images undoState imageCropMode clipboard_ processingState =
    let
        settings =
            settingsFromState processingState images

        clipboardTitle x clipboard =
            interpolate "{0} from {1}" [ x, Path.toString clipboard.filename ]
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
                        [ viewClipboardButton (clipboardTitle "tone+crop" clipboard)
                            Icon.applyBoth
                            { clipboard | filename = settings.filename }
                        , viewClipboardButton (clipboardTitle "tone" clipboard)
                            Icon.applyTone
                            { clipboard
                                | filename = settings.filename
                                , rotate = settings.rotate
                                , crop = settings.crop
                            }
                        , viewClipboardButton (clipboardTitle "crop" clipboard)
                            Icon.applyCrop
                            { settings | crop = clipboard.crop }
                        , button
                            [ onClick <|
                                ApplyCopyToAll <|
                                    Zipper.map (\i -> { clipboard | filename = i.filename }) images
                            , title (clipboardTitle "apply to all" clipboard)
                            ]
                            [ Icon.applyAll ]
                        , button
                            [ onClick <|
                                ApplyCopyToAll <|
                                    Zipper.map
                                        (\i ->
                                            { clipboard
                                                | filename = i.filename
                                                , rotate = i.rotate
                                                , crop = i.crop
                                            }
                                        )
                                        images
                            , title (clipboardTitle "apply tone to all" clipboard)
                            ]
                            [ Icon.applyAllTone ]
                        , button
                            [ onClick <|
                                ApplyCopyToAll <|
                                    Zipper.map (\i -> { i | crop = clipboard.crop }) images
                            , title (clipboardTitle "apply crop to all" clipboard)
                            ]
                            [ Icon.applyAllCrop ]
                        , button
                            [ onClick <|
                                ApplyCopyToAll <|
                                    Zipper.map (\i -> { i | rotate = clipboard.rotate }) images
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
        List.map viewSampleEvalPart offset


viewSampleEvalPart : Float -> Html Msg
viewSampleEvalPart v =
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


viewSettingsGroup : List (Html Msg) -> Html Msg
viewSettingsGroup =
    div [ class "image-settings-group" ]


viewClipboardButton : String -> Html Msg -> ImageSettings -> Html Msg
viewClipboardButton desc icon settings =
    button [ onClick (OnImageSettingsChange settings), title desc ] [ icon ]


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
            button [ onClick (UpdateImageCropMode (Just current.crop)), title "crop" ] [ Icon.crop ]

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
    -> FilmRoll
    -> Maybe ImageCrop
    -> Float
    -> ProcessingState
    -> PreviewVersions
    -> Dict ( Float, Float ) CoordinateInfo
    -> Element
    -> Html Msg
viewImage images filmRoll imageCropMode scale_ processingState previewVersions coordinateInfo element =
    let
        current =
            Zipper.current images

        ifCropping settings =
            Maybe.withDefault settings <|
                Maybe.map (\_ -> { settings | rotate = 0, crop = ImageCrop 0 0 100 })
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
                            Url.Builder.crossOrigin
                                (Path.toString filmRoll.directoryPath)
                                [ "previews", previewExtension current.filename ]
                                [ Url.Builder.int "v" <|
                                    Maybe.withDefault 0 <|
                                        Dict.Fun.get current.id previewVersions
                                ]
                        ]
                        []

                _ ->
                    img
                        [ on "load" (Decode.succeed (OnImageLoad current))
                        , onCoordinateClick
                        , id "image"
                        , style "user-select" "none"
                        , scale
                        , src <|
                            Url.Builder.absolute [ "image" ]
                                [ toImageUrlParams (ifCropping current)
                                , Url.Builder.string "dir" <|
                                    Path.toString filmRoll.directoryPath
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


viewCoordinate :
    Element
    -> ImageSettings
    -> Float
    -> CoordinateInfo
    -> ( String, Html Msg )
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


viewHistogram : Array Int -> Html msg
viewHistogram =
    Html.Keyed.node "div" [ class "histogram" ]
        << List.indexedMap viewHistogramBar
        << Array.toList


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


updateZoneByInt : Int -> (Float -> Float) -> ImageSettings -> ImageSettings
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
    { settings | zones = set settings.zones }


threeDecimalFloat : Float -> Float
threeDecimalFloat x =
    toFloat (round (x * 1000)) / 1000


previewExtension : Filename -> String
previewExtension filename =
    String.dropRight 3 (Path.toString filename) ++ "jpg"


resetAll : ImageSettings -> ImageSettings
resetAll current =
    { current
        | crop = ImageCrop 0 0 100
        , gamma = 2.2
        , zones = Zones 0 0 0 0 0 0 0 0 0
        , blackpoint = 0
        , whitepoint = 1
        , expressions = Array.empty
    }


resetTone : ImageSettings -> ImageSettings
resetTone current =
    { current
        | gamma = 2.2
        , zones = Zones 0 0 0 0 0 0 0 0 0
        , blackpoint = 0
        , whitepoint = 1
        , expressions = Array.empty
    }


emptyExpression : Expression
emptyExpression =
    Expression 0 -1 1 "" ""


toImageUrlParams : ImageSettings -> Url.Builder.QueryParameter
toImageUrlParams settings =
    -- Empty params that trigger a reload but no visual change
    Image.encodeImageSettings { settings | histogram = Array.empty }
        |> Encode.encode 0
        |> Base64.encode
        |> Url.Builder.string "image-settings"


fractionalModBy : Float -> Float -> Float
fractionalModBy m v =
    v - m * Basics.toFloat (Basics.floor (v / m))


settingsFromState : ProcessingState -> Images -> ImageSettings
settingsFromState processingState images =
    case processingState of
        Queued queuedFilmRoll ->
            Zipper.current (ProcessingState.toData queuedFilmRoll)

        _ ->
            Zipper.current images


fromArray : Array a -> Reorderable ( Maybe b, a )
fromArray =
    Array.foldl Reorderable.push Reorderable.empty
        << Array.map (Tuple.pair Nothing)


updateZipperById :
    ImageSettingsId
    -> (ImageSettings -> ImageSettings)
    -> Images
    -> Images
updateZipperById imageSettingsId f =
    Zipper.map
        (\s ->
            if s.id == imageSettingsId then
                f s

            else
                s
        )
