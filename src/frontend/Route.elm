module Route exposing
    ( BrowserParams
    , Route(..)
    , fromUrl
    , link
    , toUrl
    )

import Data.Id as Id
    exposing
        ( CollectionId
        , FilmRollId
        , ImageSettingsId
        )
import Html exposing (..)
import Html.Attributes exposing (..)
import Url exposing (Url)
import Url.Builder
import Url.Parser exposing ((</>), (<?>))
import Url.Parser.Query



-- URL


type Route
    = Browser BrowserParams
    | Editor FilmRollId ImageSettingsId


type alias BrowserParams =
    { selectedCollections : List CollectionId
    , minimumRating : Int
    }


fromUrl : Url -> Route
fromUrl url =
    let
        parser =
            Url.Parser.oneOf
                [ Url.Parser.map Editor <|
                    Url.Parser.s "editor"
                        </> Id.fromUrl
                        </> Id.fromUrl
                , Url.Parser.map Browser <|
                    Url.Parser.top
                        <?> Url.Parser.Query.map2 BrowserParams
                                collectionIdParams
                                ratingParam
                ]
    in
    Maybe.withDefault (Browser (BrowserParams [] 0)) <|
        Url.Parser.parse parser url


collectionIdParams : Url.Parser.Query.Parser (List CollectionId)
collectionIdParams =
    Url.Parser.Query.custom "collection" (List.filterMap Id.fromString)


ratingParam : Url.Parser.Query.Parser Int
ratingParam =
    Url.Parser.Query.map (Maybe.withDefault 0) <|
        Url.Parser.Query.int "rating"


toUrl : Route -> String
toUrl route =
    case route of
        Browser { minimumRating, selectedCollections } ->
            Url.Builder.absolute [] <|
                List.concat
                    [ List.map (Url.Builder.int "collection" << Id.toInt)
                        selectedCollections
                    , [ Url.Builder.int "rating" minimumRating ]
                    ]

        Editor filmRollId imageSettingsId ->
            Url.Builder.absolute
                [ "editor"
                , Id.toString filmRollId
                , Id.toString imageSettingsId
                ]
                []


link : Route -> List (Attribute msg) -> List (Html msg) -> Html msg
link route attributes content =
    a (href (toUrl route) :: attributes) content
