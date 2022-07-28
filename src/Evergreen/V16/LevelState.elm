module Evergreen.V16.LevelState exposing (..)

import Evergreen.V16.Point


type Direction
    = Up
    | Left
    | Right
    | Down


type alias PlayerInstant =
    { position : Evergreen.V16.Point.Point
    , age : Int
    }


type alias BoxInstant =
    { position : Evergreen.V16.Point.Point
    }


type alias LevelInstant =
    { players : List PlayerInstant
    , boxes : List BoxInstant
    }
