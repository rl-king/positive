module Dict.Fun exposing
    ( Dict
    , empty
    , fromList
    , get
    , insert
    , union
    , update
    )

import Dict


type Dict k comparable v
    = Dict (k -> comparable) (comparable -> k) (Dict.Dict comparable v)


empty : (k -> comparable) -> (comparable -> k) -> Dict k comparable v
empty f g =
    Dict f g Dict.empty


get : k -> Dict k comparable v -> Maybe v
get k (Dict f _ dict) =
    Dict.get (f k) dict


insert : k -> v -> Dict k comparable v -> Dict k comparable v
insert k v dict =
    update k (always (Just v)) dict


update : k -> (Maybe v -> Maybe v) -> Dict k comparable v -> Dict k comparable v
update k h (Dict f g dict) =
    Dict f g <|
        Dict.update (f k) h dict


union : Dict k comparable v -> Dict k comparable v -> Dict k comparable v
union (Dict f g a) (Dict _ _ b) =
    Dict f g (Dict.union a b)


fromList :
    (k -> comparable)
    -> (comparable -> k)
    -> List ( k, v )
    -> Dict k comparable v
fromList f g list =
    let
        ins ( k, v ) acc =
            insert k v acc
    in
    List.foldl ins (empty f g) list
