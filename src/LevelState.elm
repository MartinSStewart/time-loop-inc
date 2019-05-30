module LevelState exposing (BoxStart, LevelInstant, LevelState, MoveAction(..), PlayerStart, getLevelState)

import Level exposing (Level)
import Point exposing (Point)
import Set exposing (Set)


init : Level -> List MoveAction -> LevelState
init level actions =
    { playerPrimeStart = { startPosition = level.playerStart, startTime = Nothing, actions = actions }
    , playersStart = []
    , boxesStart =
        level.boxesStart
            |> Set.toList
            |> List.map (\pos -> { startPosition = pos, startTime = Nothing })
    }


type alias LevelState =
    { playerPrimeStart : PlayerStart
    , playersStart : List PlayerStart
    , boxesStart : List BoxStart
    }


{-| Instantaneous state of a level.
-}
type alias LevelInstant =
    { playerPrime : ( Point, List MoveAction )
    , players : List ( Point, List MoveAction )
    , boxes : List Point
    }


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


getLevelState : Level -> List MoveAction -> Int -> LevelInstant
getLevelState level actions viewTime =
    let
        state =
            init level actions
    in
    { playerPrime = ( state.playerPrimeStart.startPosition, state.playerPrimeStart.actions )
    , players = []
    , boxes = state.boxesStart |> List.map .startPosition
    }


step : Level -> Int -> LevelInstant -> LevelInstant
step level time levelInstant =
    let
        ( playerPos, actionsLeft ) =
            levelInstant.playerPrime

        playerPosNext =
            movePlayer level playerPos (List.head actionsLeft |> Maybe.withDefault MoveNone)
    in
    { playerPrime = ( playerPosNext, List.drop 1 actionsLeft )
    , players = levelInstant.players
    , boxes = levelInstant.boxes
    }


earliestEvent : LevelState -> Maybe Int
earliestEvent levelState =
    List.map .startTime levelState.playersStart
        |> (::) levelState.playerPrimeStart.startTime
        |> (++) (List.map .startTime levelState.boxesStart)
        |> List.filterMap identity
        |> List.minimum


defaultStartTime =
    0


latestEvent : LevelState -> Maybe Int
latestEvent levelState =
    List.map
        (\a -> Maybe.withDefault defaultStartTime a.startTime + List.length a.actions)
        levelState.playersStart
        |> (::)
            (Maybe.withDefault defaultStartTime levelState.playerPrimeStart.startTime
                + List.length levelState.playerPrimeStart.actions
            )
        |> (++) (List.map .startTime levelState.boxesStart |> List.filterMap identity)
        |> List.minimum


movePlayer : Level -> Point -> MoveAction -> Point
movePlayer level playerPosition action =
    let
        nextPos =
            Point.add playerPosition (actionOffset action)
    in
    if Set.member nextPos level.walls then
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
