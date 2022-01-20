module LevelState exposing
    ( BoxStart
    , LevelInstant
    , MoveAction(..)
    , PlayerStart
    , instant
    )

import Level exposing (Level)
import Point exposing (Point)
import Set exposing (Set)


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


instant : Level -> Int -> List MoveAction -> LevelInstant
instant level currentTime moveActions =
    init level


init : Level -> LevelInstant
init level =
    { players = [ { position = Level.playerStart level, age = 0 } ]
    , boxes =
        Level.boxesStart level
            |> Set.toList
            |> List.map (\position -> { position = position, age = 0 })
    }


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
