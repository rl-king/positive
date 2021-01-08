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
import Dict exposing (Dict)
import Generated.Data.ImageSettings
    exposing
        ( FilmRollSettings
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
                    matchKey "p" (SetPoster dir (focusWithOffset offset filmRoll.frsSettings))
            )
            (Maybe.andThen (\( k, _ ) -> Dict.get k model.filmRolls) model.filmRollHover)
            model.filmRollHover



-- MODEL


type alias Model =
    { filmRolls : FilmRolls
    , filmRollHover : Maybe ( String, Float )
    , minimumRating : Maybe Int
    , columns : Int
    }


type alias FilmRolls =
    Dict String FilmRollSettings


init : { minimumRating : Maybe Int } -> FilmRolls -> Model
init { minimumRating } filmRolls =
    { filmRolls = filmRolls
    , filmRollHover = Nothing
    , minimumRating = minimumRating
    , columns = 4
    }



-- UPDATE


type Msg
    = OnFilmRollHoverStart String Float
    | OnFilmRollHoverMove Float
    | OnFilmRollHoverEnd
    | SetPoster String (Maybe String)
    | GotSaveImageSettings String (HttpResult FilmRollSettings)
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

        GotSaveImageSettings dir (Ok settings) ->
            ( { model | filmRolls = Dict.insert dir settings model.filmRolls }
            , Cmd.none
            )

        GotSaveImageSettings _ (Err _) ->
            ( model, Cmd.none )

        SetPoster dir poster ->
            case Dict.get dir model.filmRolls of
                Nothing ->
                    ( model, Cmd.none )

                Just filmRoll ->
                    ( model
                    , Cmd.map (GotSaveImageSettings dir) <|
                        Request.postImageSettings dir <|
                            { filmRoll | frsPoster = poster }
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
            List.head << List.filterMap String.toInt << String.words

        sorter ( a, _ ) ( b, _ ) =
            case Maybe.map2 Tuple.pair (firstNumber a) (firstNumber b) of
                Just ( x, y ) ->
                    down x y

                Nothing ->
                    down a b

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
                    [ Input.viewRangeInt SetColumnCount 1 ( 2, 13, 5 ) "Columns" model.columns
                    , Input.viewRangeInt SetMinRating 1 ( 0, 5, 0 ) "Minimum rating" <|
                        Maybe.withDefault 0 model.minimumRating
                    ]
                ]
            , case model.minimumRating of
                Nothing ->
                    div [ class "browser-filmrolls" ] <|
                        List.map (viewFilmRollBrowserRoll model.columns model.filmRollHover) <|
                            List.sortWith sorter <|
                                Dict.toList model.filmRolls

                Just n ->
                    div [ class "browser-rated" ] <|
                        List.map (viewFilmRollBrowserRated n) <|
                            List.sortWith sorter <|
                                Dict.toList model.filmRolls
            , footer []
                [ text "Total photos: "
                , text <|
                    String.fromInt <|
                        Dict.foldl (\_ v acc -> Dict.size v.frsSettings + acc) 0 model.filmRolls
                ]
            ]
        ]



-- RATED


viewFilmRollBrowserRated : Int -> ( String, FilmRollSettings ) -> Html Msg
viewFilmRollBrowserRated minimumRating ( dir, filmRoll ) =
    case List.filter ((<=) minimumRating << Tuple.second) (Dict.toList filmRoll.frsRatings) of
        [] ->
            div [] []

        rated ->
            div [ class "browser-rated-roll" ]
                [ h2 [] [ text dir ]
                , div [ class "browser-rated-images" ] <|
                    List.map (viewRatedImage dir) rated
                ]


viewRatedImage : String -> ( String, Int ) -> Html msg
viewRatedImage dir ( filename, rating ) =
    let
        gliph n =
            if n <= rating then
                Icon.starred

            else
                Icon.unstarred
    in
    a [ href (Route.toUrl (Route.Editor { dir = dir, filename = filename })) ]
        [ img [ src (toPreviewUrl dir filename) ] []
        , text filename
        , div [ class "browser-rated-rating" ] <|
            List.map (\n -> span [] [ gliph n ]) (List.range 1 5)
        ]



-- ROLLS


viewFilmRollBrowserRoll : Int -> Maybe ( String, Float ) -> ( String, FilmRollSettings ) -> Html Msg
viewFilmRollBrowserRoll columns filmRollHover ( dir, filmRoll ) =
    let
        name =
            Maybe.withDefault dir <|
                List.head (List.reverse (String.split "/" dir))

        shortTitle =
            if String.length name > 30 then
                interpolate "{0} ... {1}"
                    [ String.trim (String.left 9 name)
                    , String.trim (String.right 21 name)
                    ]

            else
                name

        poster =
            case Maybe.map (Tuple.mapFirst ((==) dir)) filmRollHover of
                Just ( True, offset ) ->
                    focusWithOffset offset filmRoll.frsSettings

                _ ->
                    Maybe.map .iFilename <|
                        choice
                            [ Maybe.andThen (\filename -> Dict.get filename filmRoll.frsSettings) filmRoll.frsPoster
                            , List.head (Dict.values filmRoll.frsSettings)
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


viewFilmRollBrowserImage : String -> String -> Html Msg
viewFilmRollBrowserImage dir filename =
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
        , href (Route.toUrl (Route.Editor { dir = dir, filename = filename }))
        ]
        [ span
            [ style "background-image" <|
                interpolate "url(\"{0}\")" <|
                    [ toPreviewUrl dir filename ]
            ]
            []
        ]


toPreviewUrl : String -> String -> String
toPreviewUrl dir filename =
    let
        previewExtension x =
            String.dropRight 3 x ++ "jpg"
    in
    Url.Builder.absolute
        [ dir, "previews", previewExtension filename ]
        []



-- HELPERS


focusWithOffset : Float -> Dict String ImageSettings -> Maybe String
focusWithOffset offset xs =
    List.drop (round (toFloat (Dict.size xs) * offset) - 1) (Dict.values xs)
        |> List.head
        |> Maybe.map .iFilename
