module Route exposing
    ( EditorRoute
    , Route(..)
    , fromUrl
    , toUrl
    )

import String.Interpolate exposing (interpolate)
import Url exposing (Url)
import Url.Builder
import Url.Parser exposing ((</>), (<?>))
import Url.Parser.Query



-- URL


type Route
    = Browser { minimumRating : Maybe Int }
    | Editor EditorRoute
    | DecodeError String


type alias EditorRoute =
    { dir : String
    , filename : String
    }


fromUrl : Url -> Route
fromUrl url =
    let
        toEditorRoute a b =
            Maybe.withDefault (DecodeError (interpolate "Failed to decode {0} {1}" [ a, b ])) <|
                Maybe.map2 (\x y -> Editor { dir = x, filename = y })
                    (Url.percentDecode a)
                    (Url.percentDecode b)

        parser =
            Url.Parser.oneOf
                [ Url.Parser.map toEditorRoute <|
                    Url.Parser.s "editor"
                        </> Url.Parser.string
                        </> Url.Parser.string
                , Url.Parser.map (toEditorRoute ".") <|
                    Url.Parser.s "editor"
                        </> Url.Parser.string
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

        Editor { dir, filename } ->
            Url.Builder.absolute [ "editor", Url.percentEncode dir, Url.percentEncode filename ] []

        DecodeError _ ->
            Url.Builder.absolute [] []
