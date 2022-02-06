module Evergreen.V12.LevelState exposing (..)

import Evergreen.V12.Point


type Direction
    = Up
    | Left
    | Right
    | Down


type alias PlayerInstant =
    { position : Evergreen.V12.Point.Point
    , age : Int
    }


type alias BoxInstant =
    { position : Evergreen.V12.Point.Point
    }


type alias LevelInstant =
    { players : List PlayerInstant
    , boxes : List BoxInstant
    }
