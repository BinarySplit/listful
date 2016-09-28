module ListTools exposing (..)

import List exposing (head, indexedMap)


indexOf : n -> List n -> Maybe Int
indexOf value list =
    list
        |> indexedMap (,)
        |> List.filter (\( k, v ) -> v == value)
        |> List.map (\( k, v ) -> k)
        |> head


insert : Int -> n -> List n -> List n
insert i v list =
    (List.take i list) ++ [ v ] ++ (List.drop i list)


removeElem : n -> List n -> List n
removeElem elem list =
    List.filter ((/=) elem) list


getAt : List a -> Int -> Maybe a
getAt xs idx =
    List.head <| List.drop idx xs
