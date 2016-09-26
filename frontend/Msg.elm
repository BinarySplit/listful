module Msg exposing (..)


type Msg
    = SetContent Int String
    | Insert Int
    | Indent Int
    | Unindent Int
    | NoOp
