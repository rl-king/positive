port module Main exposing (main)

import Browser
import Browser.Navigation as Navigation
import Data.Id as Id
import Dict exposing (Dict)
import Dict.Fun
import Generated.Data as Image exposing (Filename(..), FilmRoll)
import Generated.Request as Request
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import List.Zipper as Zipper
import Page.Browser
import Page.Editor
import Route exposing (Route)
import ScrollTo
import String.Interpolate exposing (interpolate)
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
    { filmRolls : Status (List FilmRoll)
    , page : Page
    , key : Navigation.Key
    , scrollTo : ScrollTo.State
    , notifications : List ( Level, String )
    }


type Page
    = Browser Page.Browser.Model
    | Editor Page.Editor.Model
    | Loading


init : () -> Url.Url -> Navigation.Key -> ( Model, Cmd Msg )
init _ url key =
    onNavigation (Route.fromUrl url)
        { filmRolls = Unknown
        , page = Loading
        , key = key
        , scrollTo = ScrollTo.init
        , notifications = []
        }



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | ScrollToMsg ScrollTo.Msg
    | CancelScroll
    | GotFilmRolls Route (HttpResult (List FilmRoll))
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
            onNavigation (Route.fromUrl url) model

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
                { model | filmRolls = Success filmRolls }

        GotFilmRolls _ (Err _) ->
            pushNotification Warning
                RemoveNotification
                "Error gettings filmroll settings"
                { model | filmRolls = Failure }

        RemoveNotification ->
            ( { model
                | notifications =
                    List.take (List.length model.notifications - 1)
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


mapPage : Model -> (a -> Page) -> (msg -> Msg) -> ( a, Cmd msg ) -> ( Model, Cmd Msg )
mapPage model toPage toMsg ( page, cmds ) =
    ( { model | page = toPage page }
    , Cmd.map toMsg cmds
    )


onNavigation : Route -> Model -> ( Model, Cmd Msg )
onNavigation route model =
    let
        sortFun =
            Image.filenameToString << .filename

        toSortedZipper filmRoll =
            Zipper.fromList (List.sortBy sortFun filmRoll.imageSettings)
                |> Maybe.map (Tuple.pair filmRoll)

        toFilmRoll filmRollId filmRolls =
            List.filter ((/=) filmRollId << .id) filmRolls
                |> List.head
                |> Maybe.andThen toSortedZipper
    in
    checkScrollPosition model.page <|
        case .filmRolls (extractUpdates model) of
            Requested ->
                ( model, Cmd.none )

            Failure ->
                pushNotification Warning RemoveNotification "Error loading filmrolls" model

            Unknown ->
                ( { model | filmRolls = Requested }
                , Cmd.map (GotFilmRolls route) Request.getImageSettings
                )

            Success filmRolls ->
                case route of
                    Route.Browser data ->
                        ( { model | page = Browser (Page.Browser.init data filmRolls) }
                        , Cmd.none
                        )

                    Route.Editor filmRollId imageSettingsId ->
                        case toFilmRoll filmRollId filmRolls of
                            Nothing ->
                                pushNotification Warning RemoveNotification "Error loading filmroll" model

                            Just ( filmRoll, images ) ->
                                case model.page of
                                    Editor m ->
                                        ( { model
                                            | page =
                                                Editor (Page.Editor.continue imageSettingsId images m)
                                          }
                                        , Cmd.map ScrollToMsg ScrollTo.scrollToTop
                                        )

                                    _ ->
                                        ( { model
                                            | page =
                                                Editor (Page.Editor.init filmRoll imageSettingsId images)
                                          }
                                        , Cmd.map ScrollToMsg ScrollTo.scrollToTop
                                        )

                    Route.DecodeError err ->
                        pushNotification Warning RemoveNotification err model


extractUpdates : Model -> Model
extractUpdates model =
    let
        mapStatus f status =
            case status of
                Success x ->
                    Success (f x)

                x ->
                    x

        fromZipper xs =
            Dict.Fun.fromList Image.filenameToString Filename <|
                List.map (\x -> ( x.filename, x )) <|
                    Zipper.toList xs
    in
    case model.page of
        Loading ->
            model

        Browser m ->
            { model | filmRolls = mapStatus (always m.filmRolls) model.filmRolls }

        Editor m ->
            { model
                | filmRolls =
                    mapStatus
                        ((::) m.filmRoll << List.filter ((/=) m.filmRoll.id << .id))
                        model.filmRolls
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
            ( model, Cmd.batch [ cmd, Cmd.map ScrollToMsg ScrollTo.scrollToTop ] )

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
                ]
            }

        Editor m ->
            { title =
                interpolate "{0} | {1} | Editor"
                    [ "todo", "todo" ]
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
                , on "click" (Decode.succeed CancelScroll)
                , class "scroll-cancel-overlay"
                ]
                []
