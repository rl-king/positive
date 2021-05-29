module Route exposing
    ( BrowserParams
    , Columns(..)
    , Rating(..)
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
    , columns : Columns
    , minimumRating : Rating
    }


type Columns
    = Columns Int


type Rating
    = Rating Int


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
                        <?> Url.Parser.Query.map3 BrowserParams
                                collectionIdParams
                                columnsParam
                                ratingParam
                ]
    in
    Maybe.withDefault (Browser (BrowserParams [] (Columns 6) (Rating 0))) <|
        Url.Parser.parse parser url


collectionIdParams : Url.Parser.Query.Parser (List CollectionId)
collectionIdParams =
    Url.Parser.Query.custom "collection" (List.filterMap Id.fromString)


ratingParam : Url.Parser.Query.Parser Rating
ratingParam =
    Url.Parser.Query.map (Rating << Maybe.withDefault 0) <|
        Url.Parser.Query.int "rating"


columnsParam : Url.Parser.Query.Parser Columns
columnsParam =
    Url.Parser.Query.map (Columns << Maybe.withDefault 6) <|
        Url.Parser.Query.int "columns"


toUrl : Route -> String
toUrl route =
    case route of
        Browser { minimumRating, selectedCollections, columns } ->
            let
                ( Rating minimumRating_, Columns columns_ ) =
                    ( minimumRating, columns )
            in
            Url.Builder.absolute [] <|
                List.concat
                    [ List.map (Url.Builder.int "collection" << Id.toInt)
                        selectedCollections
                    , [ Url.Builder.int "rating" minimumRating_
                      , Url.Builder.int "columns" columns_
                      ]
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
