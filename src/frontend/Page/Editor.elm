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
import Browser.Dom exposing (Element)
import Browser.Events
import Browser.Navigation as Navigation
import Dict exposing (Dict)
import Generated.Data.ImageSettings as ImageSettings
    exposing
        ( CoordinateInfo
        , FilmRollSettings
        , ImageCrop
        , ImageSettings
        , Zones
        )
import Generated.Request as Request
import Html exposing (..)
import Html.Attributes as Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed
import Html.Lazy
import Icon
import Input
import Json.Decode as Decode
import Json.Encode as Encode
import List.Zipper as Zipper exposing (Zipper)
import Process
import ProcessingState exposing (ProcessingState(..))
import String.Interpolate exposing (interpolate)
import Task
import Url.Builder
import Util exposing (..)



-- PORT


port previewReady : (String -> msg) -> Sub msg



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions { imageCropMode, clipboard, filmRoll } =
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
        , Browser.Events.onResize (\_ _ -> OnBrowserResize)
        ]



-- MODEL


type alias Model =
    { processingState : ProcessingState
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
    , route : EditorRoute
    , notifications : List ( Level, String )
    , imageElement : Element
    , coordinateInfo : Dict ( Float, Float ) CoordinateInfo
    }


type alias FilmRoll =
    Zipper ImageSettings


type alias Ratings =
    Dict String Int


type alias PreviewVersions =
    Dict String Int


init : EditorRoute -> FilmRoll -> Ratings -> Maybe String -> Model
init route filmRoll ratings poster =
    { processingState = ProcessingState.preview
    , ratings = ratings
    , poster = poster
    , filmRoll = focus route filmRoll
    , saveKey = Key 0
    , imageCropMode = Nothing
    , clipboard = Nothing
    , histogram = List.repeat 255 0
    , undoState = []
    , scale = 1
    , minimumRating = 0
    , previewColumns = 4
    , route = route
    , notifications = []
    , previewVersions = Dict.empty
    , imageElement =
        { scene = { width = 0, height = 0 }
        , viewport = { x = 0, y = 0, width = 0, height = 0 }
        , element = { x = 0, y = 0, width = 0, height = 0 }
        }
    , coordinateInfo = Dict.empty
    }


continue : EditorRoute -> FilmRoll -> Ratings -> Maybe String -> Model -> Model
continue route filmRoll ratings poster model =
    { model
        | processingState = ProcessingState.preview
        , ratings = ratings
        , poster = poster
        , filmRoll = focus route filmRoll
        , route = route
        , coordinateInfo = Dict.empty
    }


focus : EditorRoute -> FilmRoll -> FilmRoll
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
    | OnImageClick ( Float, Float )
    | RemoveCoordinate CoordinateInfo
    | RequestCoordinateInfo ( Float, Float ) Element
    | GotCoordinateInfo Element (HttpResult (List CoordinateInfo))
    | OnBrowserResize
    | GotElementPosition Element
    | OpenExternalEditor
    | GotOpenExternalEditor (HttpResult ())


update : Navigation.Key -> Msg -> Model -> ( Model, Cmd Msg )
update key msg model =
    case msg of
        GotHistogram result ->
            ( { model | histogram = Result.withDefault [] result }, Cmd.none )

        GotSaveImageSettings _ (Ok _) ->
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
            if Just settings == model.clipboard then
                ( { model | clipboard = Nothing }, Cmd.none )

            else
                ( { model | clipboard = Just settings, imageCropMode = Nothing }, Cmd.none )

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
                        Cmd.map (GotCoordinateInfo model.imageElement) <|
                            Request.postImageSettingsCoordinate model.route.dir
                                ( Dict.keys model.coordinateInfo
                                , Zipper.current model.filmRoll
                                )
            in
            case model.processingState of
                Preview state ->
                    ( model, Cmd.none )

                Ready state ->
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
            ( { model | undoState = [], saveKey = nextKey model.saveKey }
            , Navigation.pushUrl key <|
                (toUrl << Editor << (\x -> { dir = model.route.dir, filename = x.iFilename }) << Zipper.current) <|
                    Maybe.withDefault (Zipper.last model.filmRoll) (Zipper.previous model.filmRoll)
            )

        NextImage ->
            ( { model | undoState = [], saveKey = nextKey model.saveKey }
            , Navigation.pushUrl key <|
                (toUrl << Editor << (\x -> { dir = model.route.dir, filename = x.iFilename }) << Zipper.current) <|
                    Maybe.withDefault (Zipper.first model.filmRoll) (Zipper.next model.filmRoll)
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

        AttemptSave _ saveKey _ ->
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
                , coordinateInfo = Dict.insert ( cX, cY ) (CoordinateInfo cX cY 0) model.coordinateInfo
              }
            , Cmd.map (GotCoordinateInfo imageElement) <|
                Request.postImageSettingsCoordinate model.route.dir
                    ( [ ( cX, cY ) ], Zipper.current model.filmRoll )
            )

        GotCoordinateInfo _ (Ok coordinateInfo) ->
            ( { model
                | coordinateInfo =
                    List.foldl (\c -> Dict.insert ( c.ciX, c.ciY ) c) model.coordinateInfo coordinateInfo
              }
            , Cmd.none
            )

        GotCoordinateInfo _ (Err _) ->
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
                Request.postImageSettingsExternaleditor model.route.dir (Zipper.current model.filmRoll)
            )

        GotOpenExternalEditor _ ->
            ( model, Cmd.none )


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


fromPreview : ProcessingState -> ProcessingState
fromPreview state =
    case state of
        Preview s ->
            ProcessingState.toProcessing s

        other ->
            other


updateSettings : (ImageSettings -> ImageSettings) -> Model -> Model
updateSettings f model =
    case model.processingState of
        Preview state ->
            { model
                | processingState = ProcessingState.toProcessing state
                , filmRoll = Zipper.mapCurrent f model.filmRoll
                , undoState = model.filmRoll :: model.undoState
            }

        Ready state ->
            { model
                | processingState = ProcessingState.toProcessing state
                , filmRoll = Zipper.mapCurrent f model.filmRoll
                , undoState = model.filmRoll :: model.undoState
            }

        Processing state ->
            { model
                | processingState = ProcessingState.toQueued (Zipper.mapCurrent f model.filmRoll) state
                , undoState = model.filmRoll :: model.undoState
            }

        Queued state ->
            { model
                | processingState = ProcessingState.map (Zipper.mapCurrent f) state
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
        , Html.Lazy.lazy6 viewSettings
            model.filmRoll
            model.histogram
            model.undoState
            model.imageCropMode
            model.clipboard
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


viewNav : EditorRoute -> Html Msg
viewNav route =
    nav []
        [ a [ href "/" ] [ text "browser" ]
        , text "/"
        , text route.dir
        , text "/"
        , text route.filename
        ]



-- FILES


viewFiles : EditorRoute -> Int -> Int -> PreviewVersions -> Ratings -> FilmRoll -> Html Msg
viewFiles route columns minimumRating previewVersions ratings filmRoll =
    section [ class "files" ]
        [ Input.viewRange (SetColumnCount << floor) 1 ( 2, 13, 5 ) "Columns" (toFloat columns) -- FIXME: remove floats
        , Input.viewRange (SetMinRating << floor) 1 ( 0, 5, 0 ) "Rating" (toFloat minimumRating) -- FIXME: remove floats
        , Html.Keyed.ul [] <|
            List.map (\( _, filename, x ) -> ( filename, x )) <|
                List.filter (\( rating, _, _ ) -> rating >= minimumRating) <|
                    List.concat
                        [ List.map (viewFilesLink False route.dir columns previewVersions ratings) (Zipper.before filmRoll)
                        , [ viewFilesLink True route.dir columns previewVersions ratings (Zipper.current filmRoll) ]
                        , List.map (viewFilesLink False route.dir columns previewVersions ratings) (Zipper.after filmRoll)
                        ]
        ]


viewFilesLink : Bool -> String -> Int -> PreviewVersions -> Ratings -> ImageSettings -> ( Int, String, Html Msg )
viewFilesLink isCurrent dir columns previewVersions ratings settings =
    let
        width =
            style "width" <|
                interpolate "calc({0}% - 1rem)" [ String.fromInt (100 // columns) ]

        rotate deg =
            fractionalModBy (degrees -360) (settings.iRotate - degrees deg)
    in
    ( Maybe.withDefault 0 (Dict.get settings.iFilename ratings)
    , settings.iFilename
    , li [ classList [ ( "-current", isCurrent ), ( "-small", columns > 4 ) ], width ]
        [ a [ href (toUrl (Editor { filename = settings.iFilename, dir = dir })) ]
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
            [ button [ onClick (RotatePreview settings.iFilename (rotate 270)) ] [ text "⊤" ]
            , button [ onClick (RotatePreview settings.iFilename (rotate 180)) ] [ text "⊤" ]
            , button [ onClick (RotatePreview settings.iFilename (rotate 90)) ] [ text "⊤" ]
            ]
        , span [ class "files-file-footer" ]
            [ text settings.iFilename
            , button [ onClick (CopySettings settings) ] [ Icon.copy ]
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
                Icon.starred

            else
                Icon.unstarred
    in
    div [ class "ratings" ] <|
        List.map
            (\n -> button [ onClick (Rate filename n) ] [ gliph n ])
            (List.range 1 5)



-- IMAGE SETTINGS


viewSettings :
    FilmRoll
    -> List Int
    -> List FilmRoll
    -> Maybe ImageCrop
    -> Maybe ImageSettings
    -> ProcessingState
    -> Html Msg
viewSettings filmRoll histogram undoState imageCropMode clipboard_ processingState =
    let
        settings =
            case processingState of
                Queued queuedFilmRoll ->
                    Zipper.current (ProcessingState.toData queuedFilmRoll)

                _ ->
                    Zipper.current filmRoll

        zones =
            settings.iZones
    in
    section [ class "image-settings" ]
        [ viewSettingsGroup
            [ Html.Lazy.lazy viewHistogram histogram ]
        , viewSettingsGroup <|
            List.map (Html.map OnImageSettingsChange)
                [ Input.viewRange (\v -> { settings | iZones = { zones | z1 = v } }) 0.001 ( -0.25, 0.25, 0 ) "I" zones.z1
                , Input.viewRange (\v -> { settings | iZones = { zones | z2 = v } }) 0.001 ( -0.25, 0.25, 0 ) "II" zones.z2
                , Input.viewRange (\v -> { settings | iZones = { zones | z3 = v } }) 0.001 ( -0.25, 0.25, 0 ) "III" zones.z3
                , Input.viewRange (\v -> { settings | iZones = { zones | z4 = v } }) 0.001 ( -0.25, 0.25, 0 ) "IV" zones.z4
                , Input.viewRange (\v -> { settings | iZones = { zones | z5 = v } }) 0.001 ( -0.25, 0.25, 0 ) "V" zones.z5
                , Input.viewRange (\v -> { settings | iZones = { zones | z6 = v } }) 0.001 ( -0.25, 0.25, 0 ) "VI" zones.z6
                , Input.viewRange (\v -> { settings | iZones = { zones | z7 = v } }) 0.001 ( -0.25, 0.25, 0 ) "VII" zones.z7
                , Input.viewRange (\v -> { settings | iZones = { zones | z8 = v } }) 0.001 ( -0.25, 0.25, 0 ) "VIII" zones.z8
                , Input.viewRange (\v -> { settings | iZones = { zones | z9 = v } }) 0.001 ( -0.25, 0.25, 0 ) "IX" zones.z9
                ]
        , viewSettingsGroup <|
            List.map (Html.map OnImageSettingsChange)
                [ Input.viewRange (\v -> { settings | iGamma = v }) 0.1 ( 0, 10, 2.2 ) "Gamma" settings.iGamma
                , Input.viewRange (\v -> { settings | iBlackpoint = v }) 0.01 ( -0.5, 0.5, 0 ) "Blackpoint" settings.iBlackpoint
                , Input.viewRange (\v -> { settings | iWhitepoint = v }) 0.01 ( 0.5, 1.5, 1 ) "Whitepoint" settings.iWhitepoint
                ]
        , div [ class "image-settings-buttons" ]
            [ viewSettingsGroup
                [ viewImageCropMode settings imageCropMode
                , button [ onClick Rotate, title "rotate" ] [ Icon.rotate ]
                ]
            , viewSettingsGroup
                [ button [ onClick (CopySettings settings), title "copy" ] [ Icon.copy ]
                , viewMaybe clipboard_ <|
                    \clipboard ->
                        div [ class "image-settings-paste" ]
                            [ viewClipboardButton (interpolate "both from {0}" [ clipboard.iFilename ])
                                Icon.applyBoth
                                { clipboard | iFilename = settings.iFilename }
                            , viewClipboardButton (interpolate "tone from {0}" [ clipboard.iFilename ])
                                Icon.applyTone
                                { clipboard
                                    | iFilename = settings.iFilename
                                    , iRotate = settings.iRotate
                                    , iCrop = settings.iCrop
                                }
                            , viewClipboardButton (interpolate "crop from {0}" [ clipboard.iFilename ])
                                Icon.applyCrop
                                { settings | iCrop = clipboard.iCrop }
                            , button
                                [ onClick <|
                                    ApplyCopyToAll <|
                                        Zipper.map (\i -> { clipboard | iFilename = i.iFilename }) filmRoll
                                , title (interpolate "apply to all from {0}" [ clipboard.iFilename ])
                                ]
                                [ Icon.applyAll ]
                            , button
                                [ onClick <|
                                    ApplyCopyToAll <|
                                        Zipper.map
                                            (\i ->
                                                { clipboard | iFilename = i.iFilename, iRotate = i.iRotate, iCrop = i.iCrop }
                                            )
                                            filmRoll
                                , title (interpolate "apply tone to all from {0}" [ clipboard.iFilename ])
                                ]
                                [ Icon.applyAllTone ]
                            , button
                                [ onClick <|
                                    ApplyCopyToAll <|
                                        Zipper.map (\i -> { i | iCrop = clipboard.iCrop }) filmRoll
                                , title (interpolate "apply crop to all from {0}" [ clipboard.iFilename ])
                                ]
                                [ Icon.applyAllCrop ]
                            , button
                                [ onClick <|
                                    ApplyCopyToAll <|
                                        Zipper.map (\i -> { i | iRotate = clipboard.iRotate }) filmRoll
                                , title (interpolate "apply rotate to all from {0}" [ clipboard.iFilename ])
                                ]
                                [ Icon.applyAllRotate ]
                            ]
                ]
            , viewSettingsGroup
                [ button [ onClick (OnImageSettingsChange (resetAll settings)), title "reset" ] [ Icon.reset ]
                , button [ onClick (OnImageSettingsChange (resetTone settings)), title "reset tone" ] [ Icon.resetTone ]
                , viewIf (not (List.isEmpty undoState)) <|
                    \_ -> button [ onClick Undo, title "undo" ] [ Icon.undo ]
                ]
            , viewSettingsGroup
                [ button [ onClick SaveSettings, title "save" ] [ Icon.save ]
                , button [ onClick GenerateHighres, title "generate highres" ] [ Icon.highres ]
                , button [ onClick GenerateWallpaper, title "generate wallpaper" ] [ Icon.wallpaper ]
                , button [ onClick OpenExternalEditor, title "open external" ] [ Icon.externalEditor ]
                , viewIf (processingState == ProcessingState.preview) <|
                    \_ -> button [ onClick LoadOriginal, title "load original" ] [ Icon.original ]
                ]
            , viewSettingsGroup
                [ button [ onClick NextImage ] [ Icon.right ]
                , button [ onClick PreviousImage ] [ Icon.left ]
                ]
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
            button [ onClick (UpdateImageCropMode (Just current.iCrop)) ] [ Icon.crop ]

        Just imageCrop ->
            div []
                [ button [ onClick (UpdateImageCropMode Nothing) ] [ Icon.crop ]
                , div [ class "crop-settings" ]
                    [ Input.viewRange (onTopChange imageCrop) 0.01 ( 0, 5, 0 ) "Top" imageCrop.icTop
                    , Input.viewRange (onLeftChange imageCrop) 0.01 ( 0, 5, 0 ) "Left" imageCrop.icLeft
                    , Input.viewRange (onWidthChange imageCrop) 0.1 ( 85, 100, 100 ) "Width" imageCrop.icWidth
                    , button [ onClick (UpdateImageCropMode Nothing) ] [ Icon.cancel ]
                    , button [ onClick (ApplyCrop imageCrop) ] [ Icon.ok ]
                    ]
                ]



-- IMAGE


viewImage :
    FilmRoll
    -> EditorRoute
    -> Maybe ImageCrop
    -> Float
    -> ProcessingState
    -> Dict String Int
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
                                    Maybe.withDefault 0 (Dict.get current.iFilename previewVersions)
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
        , section [ class "zones" ]
            [ Input.viewRange UpdateScale 0.01 ( 0.05, 1.05, 1 ) "Zoom" scale_ ]
        ]


viewCoordinate : Element -> ImageSettings -> Float -> CoordinateInfo -> ( String, Html Msg )
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
                        updateZoneByInt (round (coordinate.ciValue * 10)) (\v -> v - 0.025) settings
                ]
                [ text "-" ]
            , button
                [ class "coordinate-plus"
                , onClick <|
                    OnImageSettingsChange <|
                        updateZoneByInt (round (coordinate.ciValue * 10)) (\v -> v + 0.025) settings
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
    { settings | iZones = set settings.iZones }


threeDecimalFloat : Float -> Float
threeDecimalFloat x =
    toFloat (round (x * 1000)) / 1000


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
