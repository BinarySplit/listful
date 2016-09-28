module Msg exposing (..)


type Msg
    = SetContent Int String
    | SetFocus (Maybe Int)
    | InsertNode Int
    | Indent Int
    | Unindent Int
    | MoveUp Int
    | MoveDown Int
    | NoOp
