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
import Date
import Dict
import Dict.Fun
import Generated.Data exposing (Collection, FilmRoll, ImageSettings)
import Generated.Request as Request
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Icon
import Input
import Json.Decode as Decode
import Route exposing (Columns(..), Rating(..))
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
    , minimumRating : Rating
    , columns : Route.Columns
    , collections : List Collection
    , selectedCollections : List CollectionId
    }


init : Route.BrowserParams -> List Collection -> List FilmRoll -> Model
init { minimumRating, selectedCollections, columns } collections filmRolls =
    { filmRolls = filmRolls
    , filmRollHover = Nothing
    , minimumRating = minimumRating
    , columns = columns
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
    | SetMinRating Rating
    | SetColumnCount Route.Columns
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
                        , columns = model.columns
                        , selectedCollections = model.selectedCollections
                        }
            )

        SetColumnCount columns ->
            ( model
            , Browser.Navigation.pushUrl key <|
                Route.toUrl <|
                    Route.Browser
                        { minimumRating = model.minimumRating
                        , columns = columns
                        , selectedCollections = model.selectedCollections
                        }
            )

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
        images =
            imageLookupTable model.minimumRating model.filmRolls

        ( Rating minimumRating, Columns columns ) =
            ( model.minimumRating, model.columns )
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
                            columns
                            (SetColumnCount << Columns)
                        , Input.viewRangeInt 1
                            ( 0, 5, 0 )
                            "Minimum rating"
                            minimumRating
                            (SetMinRating << Rating)
                        ]
                    , viewCollectionButtons model.minimumRating
                        model.columns
                        model.selectedCollections
                        model.collections
                        images
                    ]
                ]
            , case ( model.minimumRating, model.selectedCollections ) of
                ( Rating 0, [] ) ->
                    div [] <|
                        List.map (viewFilmRollBrowserYear model.columns model.filmRollHover) <|
                            groupByYear
                                model.filmRolls

                ( n, [] ) ->
                    div [ class "browser-rated" ] <|
                        List.map (viewFiltered n model.columns) <|
                            sortByDateDesc model.filmRolls

                ( n, selectedCollections ) ->
                    viewCollections model.columns images <|
                        List.filter
                            (\{ id } -> List.member id selectedCollections)
                            model.collections
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


type alias Images =
    Dict.Fun.Dict ImageSettingsId Int ( Path.Directory, FilmRollId, ImageSettings )


imageLookupTable : Rating -> List FilmRoll -> Images
imageLookupTable (Rating minimumRating) =
    let
        empty =
            Dict.Fun.empty Id.toInt Id.fromInt

        insert filmRoll imageSettings acc =
            if imageSettings.rating >= minimumRating then
                Dict.Fun.insert imageSettings.id
                    ( filmRoll.directoryPath, filmRoll.id, imageSettings )
                    acc

            else
                acc
    in
    List.foldr
        (\filmRoll ->
            Dict.Fun.union <|
                List.foldr (insert filmRoll) empty filmRoll.imageSettings
        )
        empty


viewCollections : Columns -> Images -> List Collection -> Html Msg
viewCollections columns images collections =
    div [] <|
        List.map (viewCollection columns images) collections


viewCollection : Columns -> Images -> Collection -> Html Msg
viewCollection columns images collection =
    div [ class "browser-collection-images" ] <|
        (::) (h2 [] [ text collection.title ]) <|
            List.map (viewCollectionImage columns collection.id) <|
                List.filterMap (\id -> Dict.Fun.get id images)
                    collection.imageIds


viewCollectionImage :
    Columns
    -> CollectionId
    -> ( Path.Directory, FilmRollId, ImageSettings )
    -> Html Msg
viewCollectionImage columns collectionId ( directoryPath, filmRollId, imageSettings ) =
    div
        [ class "browser-collection-image"
        , columnWidth columns
        ]
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


viewCollectionButtons :
    Rating
    -> Columns
    -> List CollectionId
    -> List Collection
    -> Images
    -> Html Msg
viewCollectionButtons minimumRating columns selectedCollections collections images =
    div [ class "browser-controls-collections" ] <|
        List.map
            (viewCollectionSelect minimumRating
                columns
                selectedCollections
                images
            )
            collections


viewCollectionSelect :
    Rating
    -> Columns
    -> List CollectionId
    -> Images
    -> Collection
    -> Html Msg
viewCollectionSelect minimumRating columns selectedCollections images collection =
    let
        route =
            if List.member collection.id selectedCollections then
                Route.Browser
                    { minimumRating = minimumRating
                    , columns = columns
                    , selectedCollections =
                        List.filter ((/=) collection.id) selectedCollections
                    }

            else
                Route.Browser
                    { minimumRating = minimumRating
                    , columns = columns
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
        [ text <|
            interpolate "{0} ({1})"
                [ collection.title
                , String.fromInt <|
                    List.length <|
                        List.filterMap (\id -> Dict.Fun.get id images)
                            collection.imageIds
                ]
        ]



-- RATED


viewFiltered : Rating -> Columns -> FilmRoll -> Html Msg
viewFiltered (Rating minimumRating) columns filmRoll =
    case List.filter ((<=) minimumRating << .rating) filmRoll.imageSettings of
        [] ->
            div [] []

        rated ->
            div [ class "browser-rated-roll" ]
                [ h2 [] [ text (Path.toString filmRoll.directoryPath) ]
                , div [ class "browser-rated-images" ] <|
                    List.map (viewRatedImage columns filmRoll) rated
                ]


viewRatedImage : Columns -> FilmRoll -> ImageSettings -> Html msg
viewRatedImage columns filmRoll imageSettings =
    let
        gliph n =
            if n <= imageSettings.rating then
                Icon.starred

            else
                Icon.unstarred
    in
    a
        [ href (Route.toUrl (Route.Editor filmRoll.id imageSettings.id))
        , columnWidth columns
        ]
        [ img [ src (toPreviewUrl filmRoll.directoryPath imageSettings) ] []
        , text <|
            Path.toString imageSettings.filename
        , div [ class "browser-rated-rating" ] <|
            List.map (\n -> span [] [ gliph n ]) (List.range 1 5)
        ]



-- ROLLS


viewFilmRollBrowserYear :
    Columns
    -> Maybe ( FilmRoll, Float )
    -> ( Int, List FilmRoll )
    -> Html Msg
viewFilmRollBrowserYear columns filmRollHover ( year, filmRolls ) =
    div [ class "browser-filmrolls-year" ]
        [ h2 [] [ text (String.fromInt year) ]
        , div [ class "browser-filmrolls" ] <|
            List.map (viewFilmRollBrowserRoll columns filmRollHover) filmRolls
        ]


viewFilmRollBrowserRoll :
    Columns
    -> Maybe ( FilmRoll, Float )
    -> FilmRoll
    -> Html Msg
viewFilmRollBrowserRoll ((Columns columns_) as columns) filmRollHover filmRoll =
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
    in
    div
        [ classList
            [ ( "browser-filmroll", True )
            , ( "browser-filmroll-small", columns_ > 4 )
            ]
        , title (Path.toString filmRoll.directoryPath)
        , columnWidth columns
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


columnWidth : Columns -> Attribute msg
columnWidth (Columns columns) =
    style "width" <|
        interpolate "calc({0}% - 1rem)"
            [ String.fromFloat (100 / toFloat columns) ]


currentPoster : FilmRoll -> Maybe ImageSettings
currentPoster filmRoll =
    Maybe.andThen
        (\posterId ->
            List.head (List.filter ((==) posterId << .id) filmRoll.imageSettings)
        )
        filmRoll.poster


focusWithOffset : Float -> FilmRoll -> Maybe ( FilmRoll, ImageSettings )
focusWithOffset offset filmRoll =
    List.drop
        (round (toFloat (List.length filmRoll.imageSettings) * offset) - 1)
        filmRoll.imageSettings
        |> List.head
        |> Maybe.map (\imageSettings -> ( filmRoll, imageSettings ))


groupByYear : List FilmRoll -> List ( Int, List FilmRoll )
groupByYear filmRolls =
    let
        insert filmRoll =
            Dict.update (Date.year filmRoll.developedOn)
                (\maybeFilmRolls ->
                    case maybeFilmRolls of
                        Nothing ->
                            Just [ filmRoll ]

                        Just filmRolls_ ->
                            Just (sortByDateDesc (filmRoll :: filmRolls_))
                )
    in
    List.reverse <|
        List.sortBy Tuple.first <|
            Dict.toList <|
                List.foldl insert Dict.empty filmRolls


sortByDateDesc : List FilmRoll -> List FilmRoll
sortByDateDesc =
    List.reverse << List.sortWith (\a b -> Date.compare a.developedOn b.developedOn)
