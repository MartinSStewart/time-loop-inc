module Evergreen.V15.LevelState exposing (..)

import Evergreen.V15.Point


type Direction
    = Up
    | Left
    | Right
    | Down


type alias PlayerInstant =
    { position : Evergreen.V15.Point.Point
    , age : Int
    }


type alias BoxInstant =
    { position : Evergreen.V15.Point.Point
    }


type alias LevelInstant =
    { players : List PlayerInstant
    , boxes : List BoxInstant
    }
