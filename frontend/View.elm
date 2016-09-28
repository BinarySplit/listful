module View exposing (view)

import Html exposing (Html, button, div, text, input)
import Html.Attributes exposing (value, id, style)
import Html.Events exposing (on, onInput, onClick, onFocus, onBlur)
import Maybe exposing (Maybe)
import Result exposing (Result)
import Model exposing (Model, NodeList)
import Msg exposing (Msg)
import Dict
import List exposing (map, isEmpty)
import CustomEvents exposing (onKeyCombo)


view : Model -> Html Msg
view model =
    let
        childrenIds =
            Maybe.withDefault [] (Maybe.map .childrenIds (Dict.get 0 model.nodes))
    in
        if isEmpty childrenIds then
            button [ onClick (Msg.InsertNode 0) ] [ text "+" ]
        else
            div [] (map (nodeView model.nodes) childrenIds)


nodeView : NodeList -> Int -> Html Msg
nodeView nodes i =
    case (Dict.get i nodes) of
        Nothing ->
            div [] [ text "Empty" ]

        Just node ->
            div []
                [ input
                    [ onInput (Msg.SetContent i)
                    , onKeyCombo (onNodeViewKeyCombo i)
                    , onFocus (Msg.SetFocus (Just i))
                    , onBlur (Msg.SetFocus Nothing)
                    , value node.content
                    , id ("node-" ++ toString i)
                    ]
                    []
                , div [ style [ ( "margin-left", "16px" ) ] ]
                    (List.map (nodeView nodes) node.childrenIds)
                ]


onNodeViewKeyCombo : Int -> ( Bool, Bool, Bool, Int ) -> Result String Msg
onNodeViewKeyCombo i ( ctrl, alt, shift, keyCode ) =
    case ( ctrl, alt, shift, keyCode ) of
        ( _, _, _, 13 ) ->
            Ok (Msg.InsertNode i)

        ( _, _, False, 9 ) ->
            Ok (Msg.Indent i)

        ( _, _, True, 9 ) ->
            Ok (Msg.Unindent i)

        ( _, _, _, 38 ) ->
            Ok (Msg.MoveUp i)

        ( _, _, _, 40 ) ->
            Ok (Msg.MoveDown i)

        _ ->
            Err ""
