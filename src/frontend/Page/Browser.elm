module Page.Browser exposing
    ( Model
    , Msg
    , init
    , subscriptions
    , update
    , view
    )

import Browser.Events
import Dict exposing (Dict)
import Generated.Data.ImageSettings
    exposing
        ( FilmRollSettings
        , ImageCrop
        , ImageSettings
        )
import Generated.Request as Request
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import List.Zipper as Zipper exposing (Zipper)
import Set exposing (Set)
import String.Interpolate exposing (interpolate)
import Url exposing (Url)
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
    }


type alias FilmRolls =
    Dict String FilmRollSettings


init : FilmRolls -> Model
init filmRolls =
    { filmRolls = filmRolls
    , filmRollHover = Nothing
    }



-- UPDATE


type Msg
    = OnFilmRollHoverStart String Float
    | OnFilmRollHoverMove Float
    | OnFilmRollHoverEnd
    | SetPoster String (Maybe String)
    | GotSaveImageSettings String (HttpResult FilmRollSettings)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnFilmRollHoverStart id offset ->
            ( { model | filmRollHover = Just ( id, offset ) }, Cmd.none )

        OnFilmRollHoverMove offset ->
            ( { model | filmRollHover = Maybe.map (Tuple.mapSecond (always offset)) model.filmRollHover }
            , Cmd.none
            )

        OnFilmRollHoverEnd ->
            ( { model | filmRollHover = Nothing }, Cmd.none )

        GotSaveImageSettings dir (Ok settings) ->
            ( { model | filmRolls = Dict.insert dir settings model.filmRolls }, Cmd.none )

        GotSaveImageSettings _ (Err _) ->
            ( model, Cmd.none )

        SetPoster dir poster ->
            case Dict.get dir model.filmRolls of
                Nothing ->
                    ( model, Cmd.none )

                Just filmRoll ->
                    ( model
                    , Cmd.map (GotSaveImageSettings dir) <|
                        Request.postImageSettings (Url.percentEncode dir) <|
                            { filmRoll | frsPoster = poster }
                    )



-- VIEW


view : Model -> Html Msg
view model =
    main_ [] [ viewFilmRollBrowser model.filmRollHover model.filmRolls ]



-- FILE BROWSER


viewFilmRollBrowser : Maybe ( String, Float ) -> FilmRolls -> Html Msg
viewFilmRollBrowser filmRollHover filmRolls =
    let
        down ( a, _ ) ( b, _ ) =
            case compare a b of
                GT ->
                    LT

                EQ ->
                    EQ

                LT ->
                    GT
    in
    section [ class "browser" ] <|
        [ h1 [] [ text "Browser" ]
        , div [ class "browser-filmrolls" ] <|
            List.map (viewFilmRollBrowserRoll filmRollHover) <|
                List.sortWith down <|
                    Dict.toList filmRolls
        ]


viewFilmRollBrowserRoll : Maybe ( String, Float ) -> ( String, FilmRollSettings ) -> Html Msg
viewFilmRollBrowserRoll filmRollHover ( dir, filmRoll ) =
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
    in
    div [ class "browser-filmroll", title dir ]
        [ viewMaybe poster <|
            viewFilmRollBrowserImage dir
        , h2 [] [ text shortTitle ]
        ]


viewFilmRollBrowserImage : String -> String -> Html Msg
viewFilmRollBrowserImage dir filename =
    let
        previewExtension x =
            String.dropRight 3 x ++ "jpg"

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
        , href (toUrl { dir = dir, filename = filename })
        ]
        [ span
            [ style "background-image" <|
                interpolate "url(\"{0}\")" <|
                    [ Url.Builder.absolute
                        [ dir, "previews", previewExtension filename ]
                        []
                    ]
            ]
            []
        ]



-- HELPERS


focusWithOffset : Float -> Dict String ImageSettings -> Maybe String
focusWithOffset offset xs =
    List.drop (floor (toFloat (Dict.size xs) * offset)) (Dict.values xs)
        |> List.head
        |> Maybe.map .iFilename
