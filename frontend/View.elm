module View exposing (view)

import Html exposing (Html, button, div, text, input)
import Html.Attributes exposing (value)
import Html.Events exposing (onInput, onClick, on)
import Maybe exposing (Maybe)
import Result exposing (Result)
import Model exposing (Model)
import Msg exposing (Msg)
import Dict
import List exposing (map, isEmpty)
import CustomEvents exposing (onKeyCombo)


view : Model -> Html Msg
view model =
    let
        childrenIds =
            Maybe.withDefault [] (Maybe.map .childrenIds (Dict.get 0 model))
    in
        if isEmpty childrenIds then
            button [ onClick (Msg.Insert 0) ] [ text "+" ]
        else
            div [] (map (nodeView model) childrenIds)


nodeView : Model -> Int -> Html Msg
nodeView model i =
    case (Dict.get i model) of
        Nothing ->
            div [] [ text "Empty" ]

        Just node ->
            div []
                [ input [ onInput (Msg.SetContent i), onKeyCombo (onNodeViewKeyCombo i), value node.content ] []
                , div [] (List.map (nodeView model) node.childrenIds)
                ]


onNodeViewKeyCombo : Int -> ( Bool, Bool, Bool, Int ) -> Result String Msg
onNodeViewKeyCombo i ( ctrl, alt, shift, keyCode ) =
    case ( ctrl, alt, shift, keyCode ) of
        ( _, _, _, 13 ) ->
            Ok (Msg.Insert i)

        ( _, _, False, 9 ) ->
            Ok (Msg.Indent i)

        ( _, _, True, 9 ) ->
            Ok (Msg.Unindent i)

        _ ->
            Err ""
