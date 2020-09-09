module Page.Browser exposing
    ( Model
    , Msg
    , init
    , subscriptions
    , update
    , view
    )

import Base64
import Browser
import Browser.Dom
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
import ScrollTo
import Set exposing (Set)
import String.Interpolate exposing (interpolate)
import Task
import Time
import Url exposing (Url)
import Url.Builder
import Url.Parser
import Url.Parser.Query
import Util exposing (toUrl)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Maybe.withDefault Sub.none <|
        Maybe.map2
            (\filmRoll ( dir, offset ) ->
                Browser.Events.onKeyDown <|
                    matchKey "p" (SetPoster dir { filmRoll | settings = focusWithOffset offset filmRoll.settings })
            )
            (Maybe.andThen (\( k, _ ) -> Dict.get k model.filmRolls) model.filmRollHover)
            model.filmRollHover



-- MODEL


type alias Model =
    { filmRolls : FilmRolls
    , filmRollHover : Maybe ( String, Float )
    }


type alias FilmRolls =
    Dict String FilmRoll


type alias FilmRoll =
    { settings : Zipper ImageSettings
    , poster : Maybe String
    , starred : Starred
    }


type alias Starred =
    Set String


type alias Route =
    { dir : String
    , filename : String
    }


init : Dict String FilmRollSettings -> Model
init filmRolls =
    let
        toSortedZipper ( k, filmRoll ) =
            Zipper.fromList (List.sortBy .iFilename (Dict.values filmRoll.frsSettings))
                |> Maybe.map
                    (\x ->
                        ( k
                        , { settings =
                                Maybe.withDefault x <|
                                    Maybe.andThen (\poster -> Zipper.findFirst ((==) poster << .iFilename) x)
                                        filmRoll.frsPoster
                          , poster = filmRoll.frsPoster
                          , starred = filmRoll.frsStarred
                          }
                        )
                    )
    in
    { filmRolls =
        Dict.fromList <|
            List.filterMap toSortedZipper <|
                Dict.toList filmRolls
    , filmRollHover = Nothing
    }



-- UPDATE


type Msg
    = OnFilmRollHoverStart String Float
    | OnFilmRollHoverMove Float
    | OnFilmRollHoverEnd
    | SetPoster String FilmRoll


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

        SetPoster dir filmRoll ->
            ( model
            , Cmd.none
              -- , Cmd.map (GotSaveImageSettings dir) <|
              --     Request.postImageSettings (Url.percentEncode dir) <|
              --         fromZipper (Just (.iFilename (Zipper.current filmRoll))) (Dict.get dir model.starred) filmRoll
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


viewFilmRollBrowserRoll : Maybe ( String, Float ) -> ( String, FilmRoll ) -> Html Msg
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

        setFocus xs =
            case Maybe.map (Tuple.mapFirst ((==) dir)) filmRollHover of
                Just ( True, offset ) ->
                    focusWithOffset offset xs

                _ ->
                    xs
    in
    div [ class "browser-filmroll", title dir ]
        [ viewFilmRollBrowserImage dir <|
            Zipper.current (setFocus filmRoll.settings)
        , h2 [] [ text shortTitle ]
        ]


viewFilmRollBrowserImage : String -> ImageSettings -> Html Msg
viewFilmRollBrowserImage dir settings =
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
        , href (toUrl { dir = dir, filename = settings.iFilename })
        ]
        [ span
            [ style "background-image" <|
                interpolate "url(\"{0}\")" <|
                    [ Url.Builder.absolute
                        [ dir, "previews", previewExtension settings.iFilename ]
                        []
                    ]
            ]
            []
        ]



-- HELPERS


focusWithOffset : Float -> Zipper ImageSettings -> Zipper ImageSettings
focusWithOffset offset xs =
    let
        ys =
            Zipper.toList xs
    in
    List.drop (round (toFloat (List.length ys) * offset)) ys
        |> List.head
        |> Maybe.andThen (\x -> Zipper.findFirst ((==) x) xs)
        |> Maybe.withDefault xs


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
