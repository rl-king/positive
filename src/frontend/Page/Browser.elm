module Page.Browser exposing
    ( Model
    , Msg
    , init
    , subscriptions
    , update
    , view
    )

import Browser.Events
import Browser.Navigation
import Data.Id as Id exposing (CollectionId, FilmRollId, ImageSettingsId)
import Data.Path as Path
import Dict.Fun
import Generated.Data exposing (Collection, FilmRoll, ImageSettings)
import Generated.Request as Request
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Icon
import Input
import Json.Decode as Decode
import Route
import String.Interpolate exposing (interpolate)
import Url.Builder
import Util exposing (..)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Maybe.withDefault Sub.none <|
        Maybe.map
            (\( filmRoll, offset ) ->
                Browser.Events.onKeyDown <|
                    matchKey "p" (SetPoster (focusWithOffset offset filmRoll))
            )
            model.filmRollHover



-- MODEL


type alias Model =
    { filmRolls : List FilmRoll
    , filmRollHover : Maybe ( FilmRoll, Float )
    , minimumRating : Int
    , columns : Int
    , collections : List Collection
    , selectedCollections : List CollectionId
    }


init : Route.BrowserParams -> List Collection -> List FilmRoll -> Model
init { minimumRating, selectedCollections } collections filmRolls =
    { filmRolls = filmRolls
    , filmRollHover = Nothing
    , minimumRating = minimumRating
    , columns = 4
    , collections = collections
    , selectedCollections = selectedCollections
    }



-- UPDATE


type Msg
    = OnFilmRollHoverStart FilmRoll Float
    | OnFilmRollHoverMove Float
    | OnFilmRollHoverEnd
    | SetPoster (Maybe ( FilmRoll, ImageSettings ))
    | GotSaveImageSettings FilmRollId (HttpResult FilmRoll)
    | GotToggleCollection (HttpResult (List Collection))
    | SetMinRating Int
    | SetColumnCount Int
    | RemoveFromCollection CollectionId ImageSettingsId


update : Browser.Navigation.Key -> Msg -> Model -> ( Model, Cmd Msg )
update key msg model =
    case msg of
        OnFilmRollHoverStart filmRoll offset ->
            ( { model | filmRollHover = Just ( filmRoll, offset ) }, Cmd.none )

        OnFilmRollHoverMove offset ->
            ( { model
                | filmRollHover =
                    Maybe.map (Tuple.mapSecond (always offset)) model.filmRollHover
              }
            , Cmd.none
            )

        OnFilmRollHoverEnd ->
            ( { model | filmRollHover = Nothing }, Cmd.none )

        GotSaveImageSettings id (Ok filmRoll) ->
            ( { model
                | filmRolls =
                    filmRoll :: List.filter ((/=) id << .id) model.filmRolls
              }
            , Cmd.none
            )

        GotSaveImageSettings _ (Err _) ->
            ( model, Cmd.none )

        GotToggleCollection (Ok collections) ->
            ( { model | collections = collections }, Cmd.none )

        GotToggleCollection (Err _) ->
            ( model, Cmd.none )

        SetPoster Nothing ->
            ( model, Cmd.none )

        SetPoster (Just ( filmRoll, imageSettings )) ->
            ( model
            , Cmd.map (GotSaveImageSettings filmRoll.id) <|
                Request.postFilmroll <|
                    { filmRoll | poster = Just imageSettings.id }
            )

        SetMinRating val ->
            ( model
            , Browser.Navigation.pushUrl key <|
                Route.toUrl <|
                    Route.Browser
                        { minimumRating = val
                        , selectedCollections = model.selectedCollections
                        }
            )

        SetColumnCount scale ->
            ( { model | columns = scale }, Cmd.none )

        RemoveFromCollection collectionId imageSettingsId ->
            ( model
            , Cmd.map GotToggleCollection <|
                Request.deleteCollectionByCollectionIdByImageSettingsId
                    collectionId
                    imageSettingsId
            )



-- VIEW


view : Model -> Html Msg
view model =
    let
        firstNumber =
            List.head
                << List.filterMap String.toInt
                << String.words
                << String.concat
                << List.take 1
                << List.reverse
                << String.split "/"
                << Path.toString

        sorter a b =
            case Maybe.map2 Tuple.pair (firstNumber a.directoryPath) (firstNumber b.directoryPath) of
                Just ( x, y ) ->
                    down x y

                Nothing ->
                    down (Path.toString a.directoryPath)
                        (Path.toString b.directoryPath)

        down a b =
            case compare a b of
                GT ->
                    LT

                EQ ->
                    EQ

                LT ->
                    GT
    in
    main_ []
        [ section [ class "browser" ] <|
            [ header []
                [ h1 [] [ text "Browser" ]
                , div [ class "browser-controls" ]
                    [ div [ class "browser-controls-sliders" ]
                        [ Input.viewRangeInt 1
                            ( 2, 13, 5 )
                            "Columns"
                            model.columns
                            SetColumnCount
                        , Input.viewRangeInt 1
                            ( 0, 5, 0 )
                            "Minimum rating"
                            model.minimumRating
                            SetMinRating
                        ]
                    , viewCollectionButtons model.minimumRating
                        model.selectedCollections
                        model.collections
                    ]
                ]
            , case ( model.minimumRating, model.selectedCollections ) of
                ( 0, [] ) ->
                    div [ class "browser-filmrolls" ] <|
                        List.map (viewFilmRollBrowserRoll model.columns model.filmRollHover) <|
                            List.sortWith sorter model.filmRolls

                ( n, [] ) ->
                    div [ class "browser-rated" ] <|
                        List.map (viewFiltered n) <|
                            List.sortWith sorter model.filmRolls

                ( n, selectedCollections ) ->
                    viewCollections n
                        (List.filter
                            (\{ id } -> List.member id selectedCollections)
                            model.collections
                        )
                        (List.sortWith sorter model.filmRolls)
            , footer []
                [ text "Total photos: "
                , text <|
                    String.fromInt <|
                        List.foldl
                            (\v acc -> List.length v.imageSettings + acc)
                            0
                            model.filmRolls
                ]
            ]
        ]



-- COLLECTION


viewCollections : Int -> List Collection -> List FilmRoll -> Html Msg
viewCollections minimumRating collections filmRolls =
    let
        toTuple filmRoll imageSettings =
            ( imageSettings.id
            , ( filmRoll.directoryPath, filmRoll.id, imageSettings )
            )

        images =
            Dict.Fun.fromList Id.toInt Id.fromInt <|
                List.concatMap
                    (\filmRoll ->
                        List.map (toTuple filmRoll) <|
                            List.filter ((<=) minimumRating << .rating)
                                filmRoll.imageSettings
                    )
                    filmRolls
    in
    div [] <|
        List.map (viewCollection images) collections


viewCollection :
    Dict.Fun.Dict ImageSettingsId Int ( Path.Directory, FilmRollId, ImageSettings )
    -> Collection
    -> Html Msg
viewCollection images collection =
    div [ class "browser-collection-images" ] <|
        (::) (h2 [] [ text collection.title ]) <|
            List.map (viewCollectionImage collection.id) <|
                List.filterMap (\id -> Dict.Fun.get id images)
                    collection.imageIds


viewCollectionImage :
    CollectionId
    -> ( Path.Directory, FilmRollId, ImageSettings )
    -> Html Msg
viewCollectionImage collectionId ( directoryPath, filmRollId, imageSettings ) =
    div [ class "browser-collection-image" ]
        [ a [ href (Route.toUrl (Route.Editor filmRollId imageSettings.id)) ]
            [ img [ src (toPreviewUrl directoryPath imageSettings) ] [] ]
        , div [ class "browser-collection-image-footer" ]
            [ text <|
                Path.toString imageSettings.filename
            , button
                [ onClick <|
                    RemoveFromCollection collectionId imageSettings.id
                ]
                [ text "+" ]
            ]
        ]


viewCollectionButtons : Int -> List CollectionId -> List Collection -> Html Msg
viewCollectionButtons minimumRating selectedCollections collections =
    div [ class "browser-controls-collections" ] <|
        List.map (viewCollectionSelect minimumRating selectedCollections) collections


viewCollectionSelect : Int -> List CollectionId -> Collection -> Html Msg
viewCollectionSelect minimumRating selectedCollections collection =
    let
        route =
            if List.member collection.id selectedCollections then
                Route.Browser
                    { minimumRating = minimumRating
                    , selectedCollections =
                        List.filter ((/=) collection.id) selectedCollections
                    }

            else
                Route.Browser
                    { minimumRating = minimumRating
                    , selectedCollections =
                        collection.id :: selectedCollections
                    }
    in
    Route.link route
        [ classList
            [ ( "-selected", List.member collection.id selectedCollections )
            ]
        , class "browser-controls-collection"
        ]
        [ text collection.title ]



-- RATED


viewFiltered : Int -> FilmRoll -> Html Msg
viewFiltered minimumRating filmRoll =
    case List.filter ((<=) minimumRating << .rating) filmRoll.imageSettings of
        [] ->
            div [] []

        rated ->
            div [ class "browser-rated-roll" ]
                [ h2 [] [ text (Path.toString filmRoll.directoryPath) ]
                , div [ class "browser-rated-images" ] <|
                    List.map (viewRatedImage filmRoll) rated
                ]


viewRatedImage : FilmRoll -> ImageSettings -> Html msg
viewRatedImage filmRoll imageSettings =
    let
        gliph n =
            if n <= imageSettings.rating then
                Icon.starred

            else
                Icon.unstarred
    in
    a [ href (Route.toUrl (Route.Editor filmRoll.id imageSettings.id)) ]
        [ img [ src (toPreviewUrl filmRoll.directoryPath imageSettings) ] []
        , text <|
            Path.toString imageSettings.filename
        , div [ class "browser-rated-rating" ] <|
            List.map (\n -> span [] [ gliph n ]) (List.range 1 5)
        ]



-- ROLLS


viewFilmRollBrowserRoll : Int -> Maybe ( FilmRoll, Float ) -> FilmRoll -> Html Msg
viewFilmRollBrowserRoll columns filmRollHover filmRoll =
    let
        name =
            Maybe.withDefault (Path.toString filmRoll.directoryPath) <|
                List.head <|
                    List.reverse <|
                        String.split "/" <|
                            Path.toString filmRoll.directoryPath

        shortTitle =
            if String.length name > 30 then
                interpolate "{0} ... {1}"
                    [ String.trim (String.left 9 name)
                    , String.trim (String.right 21 name)
                    ]

            else
                name

        poster =
            case Maybe.map (Tuple.mapFirst ((==) filmRoll)) filmRollHover of
                Just ( True, offset ) ->
                    Maybe.map Tuple.second <|
                        focusWithOffset offset filmRoll

                _ ->
                    choice
                        [ currentPoster filmRoll
                        , List.head filmRoll.imageSettings
                        ]

        width =
            style "width" <|
                interpolate "calc({0}% - 1rem)"
                    [ String.fromInt (100 // columns) ]
    in
    div
        [ classList
            [ ( "browser-filmroll", True )
            , ( "browser-filmroll-small", columns > 4 )
            ]
        , title (Path.toString filmRoll.directoryPath)
        , width
        ]
        [ viewMaybe poster <|
            viewFilmRollBrowserImage filmRoll
        , h2 [] [ text shortTitle ]
        ]


viewFilmRollBrowserImage : FilmRoll -> ImageSettings -> Html Msg
viewFilmRollBrowserImage filmRoll imageSettings =
    let
        decodeOffset f =
            Decode.map2 (\a b -> f (a / b))
                (Decode.field "offsetX" Decode.float)
                (Decode.at [ "target", "offsetWidth" ] Decode.float)
    in
    a
        [ on "mouseover" <|
            decodeOffset (OnFilmRollHoverStart filmRoll)
        , on "mousemove" <|
            decodeOffset OnFilmRollHoverMove
        , on "mouseleave" <|
            Decode.succeed OnFilmRollHoverEnd
        , href (Route.toUrl (Route.Editor filmRoll.id imageSettings.id))
        ]
        [ span
            [ style "background-image" <|
                interpolate "url(\"{0}\")" <|
                    [ toPreviewUrl filmRoll.directoryPath imageSettings ]
            ]
            []
        ]


toPreviewUrl : Path.Directory -> ImageSettings -> String
toPreviewUrl directoryPath imageSettings =
    let
        previewExtension filename =
            String.dropRight 3 (Path.toString filename) ++ "jpg"
    in
    Url.Builder.crossOrigin
        (Path.toString directoryPath)
        [ "previews", previewExtension imageSettings.filename ]
        []



-- HELPERS


currentPoster : FilmRoll -> Maybe ImageSettings
currentPoster filmRoll =
    Maybe.andThen
        (\posterId -> List.head (List.filter ((==) posterId << .id) filmRoll.imageSettings))
        filmRoll.poster


focusWithOffset : Float -> FilmRoll -> Maybe ( FilmRoll, ImageSettings )
focusWithOffset offset filmRoll =
    List.drop (round (toFloat (List.length filmRoll.imageSettings) * offset) - 1) filmRoll.imageSettings
        |> List.head
        |> Maybe.map (\imageSettings -> ( filmRoll, imageSettings ))
