module Update exposing (update)

import Dict exposing (Dict, insert, empty, filter, keys)
import List exposing (head, member)
import Maybe exposing (Maybe)
import Debug exposing (log)
import Model exposing (Model, Node)
import Msg exposing (..)


update : Msg -> Model -> Model
update msg model =
    case (log "msg" msg) of
        SetContent i newContent ->
            { model | nodes = Dict.update i (setContent newContent) model.nodes }

        Insert i ->
            case (getParent model.nodes i) of
                Just i ->
                    Debug.crash (toString i)

                Nothing ->
                    model

        _ ->
            model


addChild : Int -> Model -> Model
addChild parentId model =
    let
        parent =
            (Dict.get parentId model.nodes)
    in
        case parent of
            Just p ->
                { model
                    | nodes =
                        model.nodes
                            |> Dict.update parentId
                                (\p ->
                                    case p of
                                        Just p ->
                                            Just { p | childrenIds = p.childrenIds :: model.nextId }

                                        Nothing ->
                                            Nothing
                                )
                            |> Dict.insert model.nextId { content = "", childrenIds = [] }
                    , nextId = 1 + 1
                }

            Nothing ->
                Debug.crash ("could not find id " ++ (toString parentId))


getParent : Dict Int Node -> Int -> Maybe Int
getParent nodes i =
    let
        parents =
            filter (\k v -> member i v.childrenIds) nodes
    in
        head (keys parents)


setContent : String -> Maybe Node -> Maybe Node
setContent newContent node =
    case node of
        Nothing ->
            Just { content = newContent, childrenIds = [] }

        Just node ->
            Just { node | content = newContent }
