module Route exposing
    ( Route(..)
    , fromUrl
    , toUrl
    )

import Data.Id exposing (FilmRollId, ImageSettingsId)
import Url exposing (Url)
import Url.Builder
import Url.Parser exposing ((</>), (<?>))
import Url.Parser.Query



-- URL


type Route
    = Browser { minimumRating : Maybe Int }
    | Editor FilmRollId ImageSettingsId


fromUrl : Url -> Route
fromUrl url =
    let
        parser =
            Url.Parser.oneOf
                [ Url.Parser.map Editor <|
                    Url.Parser.s "editor"
                        </> Data.Id.fromUrl
                        </> Data.Id.fromUrl
                , Url.Parser.map (\x -> Browser { minimumRating = x }) <|
                    Url.Parser.top
                        <?> Url.Parser.Query.int "rating"
                ]
    in
    Maybe.withDefault (Browser { minimumRating = Nothing }) <|
        Url.Parser.parse parser url


toUrl : Route -> String
toUrl route =
    case route of
        Browser { minimumRating } ->
            Url.Builder.absolute [] <|
                Maybe.withDefault [] <|
                    Maybe.map (List.singleton << Url.Builder.int "rating") minimumRating

        Editor filmRollId imageSettingsId ->
            Url.Builder.absolute
                [ "editor"
                , Data.Id.toString filmRollId
                , Data.Id.toString imageSettingsId
                ]
                []
