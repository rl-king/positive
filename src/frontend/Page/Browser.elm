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
import Data.Id exposing (FilmRollId, ImageSettingsId)
import Dict.Fun
import Generated.Data as Image
    exposing
        ( Filename(..)
        , FilmRoll
        , ImageSettings
        )
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
        Maybe.map2
            (\filmRoll ( dir, offset ) ->
                Browser.Events.onKeyDown <|
                    matchKey "p" (SetPoster (focusWithOffset offset filmRoll))
            )
            (Maybe.andThen (\( id, _ ) -> Dict.Fun.get id model.filmRolls) model.filmRollHover)
            model.filmRollHover



-- MODEL


type alias FilmRolls =
    Dict.Fun.Id { filmRollId : () } FilmRoll


type alias Model =
    { filmRolls : FilmRolls
    , filmRollHover : Maybe ( FilmRollId, Float )
    , minimumRating : Maybe Int
    , columns : Int
    }


init : { minimumRating : Maybe Int } -> FilmRolls -> Model
init { minimumRating } filmRolls =
    { filmRolls = filmRolls
    , filmRollHover = Nothing
    , minimumRating = minimumRating
    , columns = 4
    }



-- UPDATE


type Msg
    = OnFilmRollHoverStart FilmRollId Float
    | OnFilmRollHoverMove Float
    | OnFilmRollHoverEnd
    | SetPoster (Maybe ( FilmRoll, ImageSettingsId ))
    | GotSaveImageSettings FilmRollId (HttpResult FilmRoll)
    | SetMinRating Int
    | SetColumnCount Int


update : Browser.Navigation.Key -> Msg -> Model -> ( Model, Cmd Msg )
update key msg model =
    case msg of
        OnFilmRollHoverStart id offset ->
            ( { model | filmRollHover = Just ( id, offset ) }, Cmd.none )

        OnFilmRollHoverMove offset ->
            ( { model
                | filmRollHover =
                    Maybe.map (Tuple.mapSecond (always offset)) model.filmRollHover
              }
            , Cmd.none
            )

        OnFilmRollHoverEnd ->
            ( { model | filmRollHover = Nothing }, Cmd.none )

        GotSaveImageSettings id (Ok settings) ->
            ( { model | filmRolls = Dict.Fun.insert id settings model.filmRolls }
            , Cmd.none
            )

        GotSaveImageSettings _ (Err _) ->
            ( model, Cmd.none )

        SetPoster Nothing ->
            ( model, Cmd.none )

        SetPoster (Just ( filmRoll, imageSettingsId )) ->
            ( model
            , Cmd.map (GotSaveImageSettings filmRoll.filmRollId) <|
                Request.postFilmrollByFilmRollId filmRoll.filmRollId <|
                    { filmRoll | poster = Just imageSettingsId }
            )

        SetMinRating 0 ->
            ( model
            , Browser.Navigation.pushUrl key <|
                Route.toUrl (Route.Browser { minimumRating = Nothing })
            )

        SetMinRating val ->
            ( model
            , Browser.Navigation.pushUrl key <|
                Route.toUrl (Route.Browser { minimumRating = Just val })
            )

        SetColumnCount scale ->
            ( { model | columns = scale }, Cmd.none )



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

        sorter a b =
            case Maybe.map2 Tuple.pair (firstNumber a.directoryPath) (firstNumber b.directoryPath) of
                Just ( x, y ) ->
                    down x y

                Nothing ->
                    down a.directoryPath b.directoryPath

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
                    [ Input.viewRangeInt 1 ( 2, 13, 5 ) "Columns" model.columns SetColumnCount
                    , Input.viewRangeInt 1
                        ( 0, 5, 0 )
                        "Minimum rating"
                        (Maybe.withDefault 0 model.minimumRating)
                        SetMinRating
                    ]
                ]
            , case model.minimumRating of
                Nothing ->
                    div [ class "browser-filmrolls" ] <|
                        List.map (viewFilmRollBrowserRoll model.columns model.filmRollHover) <|
                            List.sortWith sorter <|
                                Dict.Fun.values model.filmRolls

                Just n ->
                    div [ class "browser-rated" ] <|
                        List.map (viewFilmRollBrowserRated n) <|
                            List.sortWith sorter <|
                                Dict.Fun.values model.filmRolls
            , footer []
                [ text "Total photos: "
                , text <|
                    String.fromInt <|
                        Dict.Fun.foldl
                            (\_ v acc -> Dict.Fun.size v.imageSettings + acc)
                            0
                            model.filmRolls
                ]
            ]
        ]



-- RATED


viewFilmRollBrowserRated : Int -> FilmRoll -> Html Msg
viewFilmRollBrowserRated minimumRating filmRoll =
    case List.filter ((<=) minimumRating << .rating) (Dict.Fun.values filmRoll.imageSettings) of
        [] ->
            div [] []

        rated ->
            div [ class "browser-rated-roll" ]
                [ h2 [] [ text filmRoll.directoryPath ]
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
    a [ href (Route.toUrl (Route.Editor filmRoll.filmRollId imageSettings.imageSettingsId)) ]
        [ img [ src (toPreviewUrl filmRoll imageSettings) ] []
        , text <|
            Image.filenameToString imageSettings.filename
        , div [ class "browser-rated-rating" ] <|
            List.map (\n -> span [] [ gliph n ]) (List.range 1 5)
        ]



-- ROLLS


viewFilmRollBrowserRoll : Int -> Maybe ( FilmRollId, Float ) -> FilmRoll -> Html Msg
viewFilmRollBrowserRoll columns filmRollHover filmRoll =
    let
        name =
            Maybe.withDefault filmRoll.directoryPath <|
                List.head (List.reverse (String.split "/" filmRoll.directoryPath))

        shortTitle =
            if String.length name > 30 then
                interpolate "{0} ... {1}"
                    [ String.trim (String.left 9 name)
                    , String.trim (String.right 21 name)
                    ]

            else
                name

        poster =
            case Maybe.map (Tuple.mapFirst ((==) filmRoll.filmRollId)) filmRollHover of
                Just ( True, offset ) ->
                    focusWithOffset offset filmRoll

                _ ->
                    Maybe.map .imageSettingsId <|
                        choice
                            [ Maybe.andThen
                                (\imageSettingsId -> Dict.Fun.get imageSettingsId filmRoll.imageSettings)
                                filmRoll.poster
                            , List.head (Dict.Fun.values filmRoll.imageSettings)
                            ]

        width =
            style "width" <|
                interpolate "calc({0}% - 1rem)" [ String.fromInt (100 // columns) ]
    in
    div
        [ classList [ ( "browser-filmroll", True ), ( "browser-filmroll-small", columns > 4 ) ]
        , title dir
        , width
        ]
        [ viewMaybe poster <|
            viewFilmRollBrowserImage dir
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
            decodeOffset (OnFilmRollHoverStart dir)
        , on "mousemove" <|
            decodeOffset OnFilmRollHoverMove
        , on "mouseleave" <|
            Decode.succeed OnFilmRollHoverEnd
        , href (Route.toUrl (Route.Editor filmRoll.filmRollId imageSettings.imageSettingsId))
        ]
        [ span
            [ style "background-image" <|
                interpolate "url(\"{0}\")" <|
                    [ toPreviewUrl filmRoll imageSettings ]
            ]
            []
        ]


toPreviewUrl : FilmRoll -> ImageSettings -> String
toPreviewUrl filmRoll imageSettings =
    let
        previewExtension (Filename x) =
            String.dropRight 3 x ++ "jpg"
    in
    Url.Builder.crossOrigin
        filmRoll.directoryPath
        [ "previews", previewExtension imageSettings.filename ]
        []



-- HELPERS


focusWithOffset : Float -> FilmRoll -> Maybe ( FilmRoll, ImageSettingsId )
focusWithOffset offset filmRoll =
    Dict.Fun.values filmRoll.imageSettings
        |> List.drop (round (toFloat (Dict.Fun.size filmRoll.imageSettings) * offset) - 1)
        |> List.head
        |> Maybe.map (\imageSettins -> ( filmRoll, imageSettins.imageSettingsId ))
