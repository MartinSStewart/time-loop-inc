module LevelState exposing (BoxStart, CurrentTimeState, LevelState, PlayerAction(..), PlayerStart, init)

import Level exposing (Level)
import Point exposing (Point)
import Set


init : Level -> LevelState
init level =
    { playerPrimeStart = { startPosition = level.playerStart, startTime = Nothing, actions = [] }
    , playersStart = []
    , boxes =
        level.boxesStart
            |> Set.toList
            |> List.map (\pos -> { startPosition = pos, startTime = Nothing })
    }


type alias LevelState =
    { playerPrimeStart : PlayerStart
    , playersStart : List PlayerStart
    , boxes : List BoxStart
    }


type alias CurrentTimeState =
    {}


type alias BoxStart =
    { startPosition : Point
    , startTime : Maybe Int
    }


type alias PlayerStart =
    { startPosition : Point
    , startTime : Maybe Int
    , actions : List PlayerAction
    }


type PlayerAction
    = MoveUp
    | MoveLeft
    | MoveRight
    | MoveDown
    | MoveNone
