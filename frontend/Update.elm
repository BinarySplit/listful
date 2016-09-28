module Update exposing (update)

import Dict exposing (Dict)
import List exposing (head, member, indexedMap)
import Maybe exposing (Maybe)
import Dom
import Model exposing (Model, NodeList, Node)
import Task
import Msg exposing (..)
import ListTools


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    Debug.log "model"
        (case (Debug.log "msg" msg) of
            SetContent i newContent ->
                setContent i newContent model ! []

            SetFocus i ->
                setFocus i model ! []

            InsertNode i ->
                insertNode i model ! [ focusOnId model.nextId ]

            MoveUp i ->
                setFocusRelative i -1 model

            MoveDown i ->
                setFocusRelative i 1 model

            Indent i ->
                indent i model ! [ focusOnId i ]

            _ ->
                model ! []
        )



-- Action Handlers


setContent : Int -> String -> Model -> Model
setContent i newContent model =
    { model
        | nodes =
            model.nodes
                |> Dict.update i (Maybe.map (\n -> { n | content = newContent }))
    }


setFocus : Maybe Int -> Model -> Model
setFocus i model =
    { model | focusId = i }


insertNode : Int -> Model -> Model
insertNode i model =
    let
        ( parentId, idx ) =
            Maybe.withDefault ( 0, 0 ) (getPosition i model)
    in
        model
            |> addChildToModel parentId (idx + 1)
            |> setFocus (Just model.nextId)


focusOnId : Int -> Cmd Msg
focusOnId id =
    Dom.focus ("node-" ++ toString id)
        |> Task.perform (\_ -> NoOp) (\_ -> NoOp)


setFocusRelative : Int -> Int -> Model -> ( Model, Cmd Msg )
setFocusRelative id m model =
    case (getNodeAtRelativePosition id m model) of
        Just fid ->
            (setFocus (Just fid) model) ! [ focusOnId fid ]

        Nothing ->
            model ! []


indent : Int -> Model -> Model
indent id model =
    case (getPosition id model) of
        Just ( parentId, pos ) ->
            if pos > 0 then
                case (getNodeAtPosition parentId (pos - 1) model) of
                    Just newParentId ->
                        model
                            |> moveNode id (Debug.log "parentId" parentId) (Debug.log "newParentId" newParentId)

                    _ ->
                        model
            else
                model

        Nothing ->
            model



-- Helper functions


addChildToModel : Int -> Int -> Model -> Model
addChildToModel parentId idx model =
    let
        insertChild =
            Maybe.map (\n -> { n | childrenIds = ListTools.insert idx model.nextId n.childrenIds })
    in
        { model
            | nodes =
                model.nodes
                    |> Dict.update parentId insertChild
                    |> Dict.insert model.nextId { content = "", childrenIds = [] }
            , nextId = model.nextId + 1
        }


moveNode : Int -> Int -> Int -> Model -> Model
moveNode id parentId newParentId model =
    { model
        | nodes =
            model.nodes
                |> Dict.update parentId
                    (Maybe.map (\n -> { n | childrenIds = ListTools.removeElem id n.childrenIds }))
                |> Dict.update newParentId
                    (Maybe.map (\n -> { n | childrenIds = n.childrenIds ++ [ id ] }))
    }


getPosition : Int -> Model -> Maybe ( Int, Int )
getPosition id model =
    let
        parentAndPos =
            model.nodes
                |> Dict.filter (\k v -> member id v.childrenIds)
                |> Dict.map (\k v -> ListTools.indexOf id v.childrenIds)
                |> Dict.toList
                |> head
    in
        case parentAndPos of
            Just ( parentId, Just pos ) ->
                Just ( parentId, pos )

            _ ->
                Nothing


getNodeAtPosition : Int -> Int -> Model -> Maybe Int
getNodeAtPosition parentId idx model =
    model.nodes
        |> Dict.get parentId
        |> (flip Maybe.andThen) (\n -> ListTools.getAt n.childrenIds idx)


linearizeNodes : NodeList -> Int -> List Int
linearizeNodes nodes i =
    case (Dict.get i nodes) of
        Just n ->
            [ i ] ++ List.concat (List.map (linearizeNodes nodes) n.childrenIds)

        Nothing ->
            []


getNodeAtRelativePosition : Int -> Int -> Model -> Maybe Int
getNodeAtRelativePosition id m model =
    let
        nodesInOrder =
            linearizeNodes model.nodes 0
    in
        ListTools.indexOf id nodesInOrder
            |> Maybe.map (\idx -> idx + m)
            |> (flip Maybe.andThen) (\idx -> ListTools.getAt nodesInOrder idx)
