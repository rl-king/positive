module Dict.Fun exposing
    ( Dict
    , Id
    , decoder
    , empty
    , encode
    , foldl
    , fromList
    , get
    , insert
    , remove
    , size
    , toList
    , update
    , values
    )

import Data.Id
import Dict
import Json.Decode as Decode
import Json.Encode as Encode


type Dict k comparable v
    = Dict (k -> comparable) (comparable -> k) (Dict.Dict comparable v)


type alias Id a v =
    Dict (Data.Id.Id a) Int v


empty : (k -> comparable) -> (comparable -> k) -> Dict k comparable v
empty f g =
    Dict f g Dict.empty


insert : k -> v -> Dict k comparable v -> Dict k comparable v
insert k v (Dict f g dict) =
    Dict f g (Dict.insert (f k) v dict)


values : Dict k comparable v -> List v
values (Dict _ _ dict) =
    Dict.values dict


size : Dict k comparable v -> Int
size (Dict _ _ dict) =
    Dict.size dict


get : k -> Dict k comparable v -> Maybe v
get k (Dict f _ dict) =
    Dict.get (f k) dict


update : k -> (Maybe v -> Maybe v) -> Dict k comparable v -> Dict k comparable v
update k h (Dict f g dict) =
    Dict f g <|
        Dict.update (f k) h dict


remove : k -> Dict k comparable v -> Dict k comparable v
remove k (Dict f g dict) =
    Dict f g (Dict.remove (f k) dict)


toList : Dict k comparable v -> List ( k, v )
toList (Dict _ g dict) =
    let
        cons k v acc =
            ( g k, v ) :: acc
    in
    Dict.foldr cons [] dict


fromList : (k -> comparable) -> (comparable -> k) -> List ( k, v ) -> Dict k comparable v
fromList f g list =
    let
        ins ( k, v ) acc =
            insert k v acc
    in
    List.foldl ins (empty f g) list


foldl : (k -> v -> b -> b) -> b -> Dict k comparable v -> b
foldl f acc (Dict _ g dict) =
    Dict.foldl (\a b c -> f (g a) b c) acc dict


decoder :
    (k -> Int)
    -> (Int -> k)
    -> Decode.Decoder v
    -> Decode.Decoder (Dict k Int v)
decoder f g =
    Decode.map (fromList f g << List.map (Tuple.mapFirst g))
        << Decode.keyValuePairs


encode : (v -> Encode.Value) -> Dict k String v -> Encode.Value
encode g (Dict _ _ dict) =
    Encode.dict identity g dict
