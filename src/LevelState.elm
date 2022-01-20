module LevelState exposing
    ( BoxStart
    , LevelInstant
    , MoveAction(..)
    , PlayerStart
    )

import Level exposing (Level)
import Point exposing (Point)
import Set exposing (Set)


init : Level -> List MoveAction -> List LevelInstant
init level actions =
    [ { players = []
      , boxes = []
      }
    ]


{-| Instantaneous state of a level.
-}
type alias LevelInstant =
    { players : List PlayerInstant
    , boxes : List BoxInstant
    }


type alias PlayerInstant =
    { position : Point, age : Int }


type alias BoxInstant =
    { position : Point, age : Int }


type alias BoxStart =
    { startPosition : Point
    , startTime : Maybe Int
    }


type alias PlayerStart =
    { startPosition : Point
    , startTime : Maybe Int
    , actions : List MoveAction
    }


type MoveAction
    = MoveUp
    | MoveLeft
    | MoveRight
    | MoveDown
    | MoveNone


step : Level -> LevelInstant -> LevelInstant
step level levelInstant =
    levelInstant


defaultStartTime =
    0


movePlayer : Level -> Point -> MoveAction -> Point
movePlayer level playerPosition action =
    let
        nextPos =
            Point.add playerPosition (actionOffset action)
    in
    if Set.member nextPos (Level.getWalls level) then
        playerPosition

    else
        nextPos


actionOffset : MoveAction -> Point
actionOffset action =
    case action of
        MoveUp ->
            ( 0, -1 )

        MoveLeft ->
            ( -1, 0 )

        MoveRight ->
            ( 1, 0 )

        MoveDown ->
            ( 0, 1 )

        MoveNone ->
            ( 0, 0 )
