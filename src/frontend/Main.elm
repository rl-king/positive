port module Main exposing (main)

import Browser
import Browser.Dom
import Browser.Events
import Browser.Navigation as Navigation
import Data.Id exposing (FilmRollId)
import Data.Path as Path
import Generated.Data exposing (Collection, FilmRoll, RollNumber(..))
import Generated.Request as Request
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import List.Zipper as Zipper exposing (Zipper)
import Page.Browser
import Page.Editor
import Route exposing (Route)
import ScrollTo
import String.Interpolate exposing (interpolate)
import Task
import Url exposing (Url)
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
        , globalNavSubscription model.filmRolls model.route
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


globalNavSubscription : Status (List FilmRoll) -> Route -> Sub Msg
globalNavSubscription status route =
    case ( status, route ) of
        ( Success filmRolls, Route.Editor filmRollId _ ) ->
            Browser.Events.onKeyDown <|
                withCtrl <|
                    Decode.oneOf
                        [ matchKey "p" <|
                            PreviousFilmRoll filmRollId filmRolls
                        , matchKey "n" <|
                            NextFilmRoll filmRollId filmRolls
                        ]

        _ ->
            Sub.none



-- MODEL


type alias Model =
    { filmRolls : Status (List FilmRoll)
    , collections : Status (List Collection)
    , route : Route
    , page : Page
    , key : Navigation.Key
    , scrollTo : ScrollTo.State
    , notifications : Notifications
    }


type Page
    = Browser Page.Browser.Model
    | Editor Page.Editor.Model
    | Loading


init : () -> Url.Url -> Navigation.Key -> ( Model, Cmd Msg )
init _ url key =
    ( { filmRolls = Unknown
      , collections = Unknown
      , route = Route.fromUrl url
      , page = Loading
      , key = key
      , scrollTo =
            ScrollTo.initWithSettings
                { strength = 200
                , dampness = 5
                }
      , notifications = emptyNotifications
      }
    , Cmd.batch
        [ Cmd.map GotFilmRolls Request.getImageSettings
        , Cmd.map GotCollections Request.getCollection
        ]
    )



-- UPDATE


type Msg
    = NoOp
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | ScrollToMsg ScrollTo.Msg
    | CancelScroll
    | GotFilmRolls (HttpResult (List FilmRoll))
    | GotCollections (HttpResult (List Collection))
    | RemoveNotification NotificationId
    | OnServerMessage String
    | EditorMsg Page.Editor.Msg
    | BrowserMsg Page.Browser.Msg
    | PreviousFilmRoll FilmRollId (List FilmRoll)
    | NextFilmRoll FilmRollId (List FilmRoll)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        LinkClicked (Browser.Internal url) ->
            ( model, Navigation.pushUrl model.key (Url.toString url) )

        LinkClicked (Browser.External href) ->
            ( model, Navigation.load href )

        UrlChanged url ->
            onNavigation <|
                extractUpdates { model | route = Route.fromUrl url }

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

        GotFilmRolls (Ok filmRolls) ->
            onNavigation <|
                { model | filmRolls = Success filmRolls }

        GotFilmRolls (Err _) ->
            pushNotification Warning
                RemoveNotification
                "Error gettings filmrolls"
                { model | filmRolls = Failure }

        GotCollections (Ok collections) ->
            onNavigation <|
                { model | collections = Success collections }

        GotCollections (Err _) ->
            pushNotification Warning
                RemoveNotification
                "Error gettings collections"
                { model | collections = Failure }

        RemoveNotification notificationId ->
            ( { model
                | notifications =
                    removeNotification notificationId
                        model.notifications
              }
            , Cmd.none
            )

        OnServerMessage message ->
            pushNotification Server RemoveNotification message model

        BrowserMsg msg_ ->
            case model.page of
                Browser m ->
                    mapPage model Browser BrowserMsg <|
                        Page.Browser.update model.key msg_ m

                _ ->
                    ( model, Cmd.none )

        EditorMsg msg_ ->
            case model.page of
                Editor m ->
                    mapPage model Editor EditorMsg <|
                        Page.Editor.update model.key msg_ m

                _ ->
                    ( model, Cmd.none )

        PreviousFilmRoll currentFilmRollId filmRolls ->
            ( model
            , moveAndSelect model.key Zipper.previous currentFilmRollId filmRolls
            )

        NextFilmRoll currentFilmRollId filmRolls ->
            ( model
            , moveAndSelect model.key Zipper.next currentFilmRollId filmRolls
            )



-- GLOBAL NAV


moveAndSelect :
    Navigation.Key
    -> (Zipper FilmRoll -> Maybe (Zipper FilmRoll))
    -> FilmRollId
    -> List FilmRoll
    -> Cmd Msg
moveAndSelect key f currentFilmRollId filmRolls =
    let
        withFirstImageId filmRoll =
            List.head (List.sortBy (Path.toString << .filename) filmRoll.imageSettings)
                |> Maybe.map (Tuple.pair filmRoll.id << .id)

        selected =
            sortByRollNumberAsc filmRolls
                |> move f ((==) currentFilmRollId << .id)
                |> Maybe.andThen withFirstImageId
    in
    case selected of
        Nothing ->
            Cmd.none

        Just ( filmRollId, imageSettingsId ) ->
            Navigation.pushUrl key <|
                Route.toUrl (Route.Editor filmRollId imageSettingsId)


move : (Zipper a -> Maybe (Zipper a)) -> (a -> Bool) -> List a -> Maybe a
move f pred xs =
    Zipper.fromList xs
        |> Maybe.andThen (Zipper.findFirst pred)
        |> Maybe.andThen f
        |> Maybe.map Zipper.current



-- NAV


mapPage : Model -> (a -> Page) -> (msg -> Msg) -> ( a, Cmd msg ) -> ( Model, Cmd Msg )
mapPage model toPage toMsg ( page, cmds ) =
    ( { model | page = toPage page }
    , Cmd.map toMsg cmds
    )


onNavigation : Model -> ( Model, Cmd Msg )
onNavigation model =
    checkScrollPosition model.page <|
        case mergeStatus model.filmRolls model.collections of
            Nothing ->
                ( model, Cmd.none )

            Just ( filmRolls, collections ) ->
                routeToPage filmRolls collections model


routeToPage : List FilmRoll -> List Collection -> Model -> ( Model, Cmd Msg )
routeToPage filmRolls collections model =
    let
        sortFun =
            Path.toString << .filename

        toSortedZipper filmRoll =
            Zipper.fromList (List.sortBy sortFun filmRoll.imageSettings)
                |> Maybe.map (Tuple.pair filmRoll)

        toFilmRoll filmRollId =
            List.filter ((==) filmRollId << .id) filmRolls
                |> List.head
                |> Maybe.andThen toSortedZipper
    in
    case model.route of
        Route.Browser data ->
            ( { model
                | page =
                    Browser (Page.Browser.init data collections filmRolls)
              }
            , Cmd.none
            )

        Route.Editor filmRollId imageSettingsId ->
            case ( toFilmRoll filmRollId, model.page ) of
                ( Nothing, _ ) ->
                    pushNotification Warning RemoveNotification "Error loading filmroll" model

                ( Just ( filmRoll, images ), Editor m ) ->
                    if filmRoll.id == m.filmRoll.id then
                        toEditor model <|
                            Page.Editor.continue imageSettingsId m

                    else
                        toEditor model <|
                            Page.Editor.init filmRoll collections imageSettingsId images

                ( Just ( filmRoll, images ), _ ) ->
                    toEditor model <|
                        Page.Editor.init filmRoll collections imageSettingsId images


toEditor : Model -> Maybe Page.Editor.Model -> ( Model, Cmd Msg )
toEditor model maybeEditor =
    case maybeEditor of
        Nothing ->
            pushNotification Warning RemoveNotification "Error init/continuing editor" model

        Just nextModel ->
            ( { model | page = Editor nextModel }, Cmd.none )


extractUpdates : Model -> Model
extractUpdates model =
    let
        mapStatus f status =
            case status of
                Success x ->
                    Success (f x)

                x ->
                    x
    in
    case model.page of
        Loading ->
            model

        Browser m ->
            { model
                | filmRolls = mapStatus (always m.filmRolls) model.filmRolls
                , collections = mapStatus (always m.collections) model.collections
            }

        Editor m ->
            let
                replace oldFilmRoll =
                    if oldFilmRoll.id == m.filmRoll.id then
                        m.filmRoll

                    else
                        oldFilmRoll
            in
            { model
                | filmRolls = mapStatus (List.map replace) model.filmRolls
                , collections = mapStatus (always m.collections) model.collections
            }



-- SCROLLTO


checkScrollPosition : Page -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
checkScrollPosition previousPage ( model, cmd ) =
    case ( previousPage, model.page ) of
        ( Editor m1, Editor m2 ) ->
            if m1.images /= m2.images then
                ( model, Cmd.batch [ cmd, Cmd.map ScrollToMsg ScrollTo.scrollToTop ] )

            else
                ( model, cmd )

        ( Browser _, Editor _ ) ->
            ( model
            , Cmd.batch
                [ cmd
                , Task.attempt (always NoOp) <|
                    Browser.Dom.setViewport 0 0
                ]
            )

        _ ->
            ( model, cmd )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    case model.page of
        Loading ->
            { title = "Loading"
            , body =
                [ viewNotifications model.notifications
                , div [ class "loading-spinner" ] []
                ]
            }

        Browser m ->
            { title = "Browser"
            , body =
                [ Html.map BrowserMsg <|
                    Page.Browser.view m
                , viewCancelScroll model.scrollTo
                , viewNotifications model.notifications
                , viewShortcuts
                ]
            }

        Editor m ->
            { title =
                interpolate "Roll {0} | {1} | Editor"
                    [ (\(RollNumber n) -> String.fromInt n) m.filmRoll.rollNumber
                    , Path.toString (Zipper.current m.images).filename
                    ]
            , body =
                [ Html.map EditorMsg <|
                    Page.Editor.view m model.notifications
                , viewCancelScroll model.scrollTo
                , viewShortcuts
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
                , on "click" (Decode.succeed CancelScroll)
                , class "scroll-cancel-overlay"
                ]
                []


viewShortcuts : Html msg
viewShortcuts =
    let
        ctrl =
            (++) "ctrl + "

        alt =
            (++) "ctrl + "
    in
    div [ class "shortcuts" ] <|
        [ h4 [] [ text "Browser" ]
        , dl [] <|
            List.map viewDef
                [ ( "p", "set poster on hover" ) ]
        , h4 [] [ text "Editor" ]
        , dl [] <|
            List.map viewDef
                [ ( alt "←", "previous image" )
                , ( alt "→", "next image" )
                , ( "a", "add to target collection" )
                , ( "r", "remove from target collection" )
                , ( ctrl "p", "previous filmroll" )
                , ( ctrl "n", "next filmroll" )
                , ( ctrl "s", "save settings" )
                , ( ctrl "u", "undo" )
                , ( ctrl "c", "copy settings" )
                , ( ctrl "r", "rotate" )
                , ( ctrl "f", "toggle fullscreen" )
                , ( ctrl "0", "reset zoom" )
                , ( ctrl "8", "zoom to 80%" )
                , ( ctrl "7", "zoom to 70%" )
                , ( ctrl "6", "zoom to 60%" )
                , ( ctrl "5", "zoom to 50%" )
                , ( ctrl "4", "zoom to 40%" )
                , ( ctrl "3", "zoom to 30%" )
                ]
        ]


viewDef : ( String, String ) -> Html msg
viewDef ( title, content ) =
    div [ class "shortcuts-def" ]
        [ dt [] [ text title ], dd [] [ text content ] ]
