port module Main exposing (main)

import Browser
import Browser.Navigation as Navigation
import Dict exposing (Dict)
import Generated.Data.ImageSettings exposing (FilmRollSettings)
import Generated.Request as Request
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import List.Zipper as Zipper exposing (Zipper)
import Page.Browser
import Page.Editor
import Process
import ScrollTo
import String.Interpolate exposing (interpolate)
import Task
import Url exposing (Url)
import Url.Parser
import Url.Parser.Query
import Util exposing (..)



-- PORTS


port serverMessage : (String -> msg) -> Sub msg



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ serverMessage OnServerMessage
        , Sub.map ScrollToMsg <|
            ScrollTo.subscriptions model.scrollTo
        , case model.page of
            Browser m ->
                Sub.map BrowserMsg <|
                    Page.Browser.subscriptions m

            Editor m ->
                Sub.map EditorMsg <|
                    Page.Editor.subscriptions m

            Loading ->
                Sub.none
        ]



-- MODEL


type alias Model =
    { filmRolls : Status FilmRolls
    , page : Page
    , key : Navigation.Key
    , scrollTo : ScrollTo.State
    , notifications : List String
    }


type Page
    = Browser Page.Browser.Model
    | Editor Page.Editor.Model
    | Loading


type Status a
    = Success a
    | Error Http.Error
    | Requested
    | Unknown


type alias FilmRolls =
    Dict String FilmRollSettings


init : () -> Url.Url -> Navigation.Key -> ( Model, Cmd Msg )
init _ url key =
    onNavigation (fromUrl url)
        { filmRolls = Unknown
        , page = Loading
        , key = key
        , scrollTo = ScrollTo.init
        , notifications = []
        }


fromUrl : Url -> Maybe Route
fromUrl url =
    let
        parser =
            Url.Parser.query <|
                Url.Parser.Query.map2 (Maybe.map2 Route)
                    (Url.Parser.Query.string "dir")
                    (Url.Parser.Query.string "filename")
    in
    Maybe.andThen identity <|
        Url.Parser.parse parser url



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | ScrollToMsg ScrollTo.Msg
    | CancelScroll
    | GotFilmRolls (Maybe Route) (HttpResult (List ( String, FilmRollSettings )))
    | RemoveNotification
    | OnServerMessage String
    | EditorMsg Page.Editor.Msg
    | BrowserMsg Page.Browser.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked (Browser.Internal url) ->
            ( model, Navigation.pushUrl model.key (Url.toString url) )

        LinkClicked (Browser.External href) ->
            ( model, Navigation.load href )

        UrlChanged url ->
            onNavigation (fromUrl url) model

        ScrollToMsg scrollToMsg ->
            let
                ( scrollToModel, scrollToCmds ) =
                    ScrollTo.update scrollToMsg model.scrollTo
            in
            ( { model | scrollTo = scrollToModel }
            , Cmd.map ScrollToMsg scrollToCmds
            )

        CancelScroll ->
            ( { model | scrollTo = ScrollTo.cancel model.scrollTo }
            , Cmd.none
            )

        GotFilmRolls route (Ok filmRolls) ->
            onNavigation route <|
                { model | filmRolls = Success (Dict.fromList filmRolls) }

        GotFilmRolls _ (Err _) ->
            pushNotification RemoveNotification "Error gettings filmroll settings" model

        RemoveNotification ->
            ( { model | notifications = List.drop 1 model.notifications }, Cmd.none )

        OnServerMessage message ->
            pushNotification RemoveNotification message model

        BrowserMsg msg_ ->
            case model.page of
                Browser m ->
                    mapPage model Browser BrowserMsg <|
                        Page.Browser.update msg_ m

                _ ->
                    ( model, Cmd.none )

        EditorMsg msg_ ->
            case model.page of
                Editor m ->
                    mapPage model Editor EditorMsg <|
                        Page.Editor.update model.key msg_ m

                _ ->
                    ( model, Cmd.none )


mapPage : Model -> (a -> Page) -> (msg -> Msg) -> ( a, Cmd msg ) -> ( Model, Cmd Msg )
mapPage model toPage toMsg ( page, cmds ) =
    ( { model | page = toPage page }
    , Cmd.map toMsg cmds
    )


onNavigation : Maybe Route -> Model -> ( Model, Cmd Msg )
onNavigation maybeRoute model =
    let
        toSortedZipper filmRoll =
            Zipper.fromList (List.sortBy .iFilename (Dict.values filmRoll.frsSettings))
                |> Maybe.map (\x -> ( x, filmRoll.frsStarred, filmRoll.frsPoster ))

        toFilmRoll filmRolls =
            Maybe.andThen (\{ dir } -> Dict.get dir filmRolls) maybeRoute
                |> Maybe.andThen toSortedZipper
                |> Maybe.map2 Tuple.pair maybeRoute
    in
    checkScrollPosition model.page <|
        case model.filmRolls of
            Requested ->
                ( model, Cmd.none )

            Error _ ->
                pushNotification RemoveNotification "Error loading filmrolls" model

            Unknown ->
                ( { model | filmRolls = Requested }
                , Cmd.map (GotFilmRolls maybeRoute) Request.getImageSettings
                )

            Success filmRolls ->
                case toFilmRoll filmRolls of
                    Nothing ->
                        ( { model | page = Browser (Page.Browser.init filmRolls) }
                        , Cmd.none
                        )

                    Just ( route, ( filmRoll, starred, poster ) ) ->
                        ( { model | page = Editor (Page.Editor.init route filmRoll starred poster) }
                        , Cmd.map ScrollToMsg ScrollTo.scrollToTop
                        )



-- SCROLLTO


checkScrollPosition : Page -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
checkScrollPosition previousPage ( model, cmd ) =
    case ( previousPage, model.page ) of
        ( Editor m1, Editor m2 ) ->
            if m1.route /= m2.route then
                ( model, Cmd.batch [ cmd, Cmd.map ScrollToMsg ScrollTo.scrollToTop ] )

            else
                ( model, cmd )

        _ ->
            ( model, Cmd.batch [ cmd, Cmd.map ScrollToMsg ScrollTo.scrollToTop ] )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    case model.page of
        Loading ->
            { title = "Loading"
            , body =
                [ viewNotification model.notifications
                ]
            }

        Browser m ->
            { title = "Browser"
            , body =
                [ Html.map BrowserMsg <|
                    Page.Browser.view m
                , viewCancelScroll model.scrollTo
                , viewNotification model.notifications
                ]
            }

        Editor m ->
            { title = m.route.filename ++ " | " ++ m.route.dir ++ " | Editor"
            , body =
                [ Html.map EditorMsg <|
                    Page.Editor.view m model.notifications
                , viewCancelScroll model.scrollTo
                ]
            }



-- NAV


viewCancelScroll : ScrollTo.State -> Html Msg
viewCancelScroll scrollTo =
    viewIf (ScrollTo.isScrolling scrollTo) <|
        \_ ->
            span
                [ on "wheel" (Decode.succeed CancelScroll)
                , on "touchstart" (Decode.succeed CancelScroll)
                , class "scroll-cancel-overlay"
                ]
                []



-- NOTIFICATONS


viewNotification : List String -> Html msg
viewNotification notifications =
    div [ class "notifications" ] <|
        List.map (\x -> span [] [ text x ]) (List.reverse notifications)
