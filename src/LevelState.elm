module LevelState exposing
    ( LevelInstant
    , MoveAction(..)
    , instant
    )

import Level exposing (Level)
import List.Extra as List
import List.Nonempty exposing (Nonempty(..))
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


type MoveAction
    = MoveUp
    | MoveLeft
    | MoveRight
    | MoveDown
    | MoveNone


step : Level -> List MoveAction -> LevelInstant -> LevelInstant
step level moveActions levelInstant =
    { levelInstant
        | players =
            List.map
                (\player ->
                    { position =
                        case List.getAt player.age moveActions of
                            Just action ->
                                Point.add (actionOffset action) player.position

                            Nothing ->
                                player.position
                    , age = player.age + 1
                    }
                )
                levelInstant.players
    }


instant : Level -> List MoveAction -> Nonempty LevelInstant
instant level moveActions =
    let
        first =
            init level
    in
    Nonempty first [ step level moveActions first ]


init : Level -> LevelInstant
init level =
    { players = [ { position = Level.playerStart level, age = 0 } ]
    , boxes =
        Level.boxesStart level
            |> Set.toList
            |> List.map (\position -> { position = position, age = 0 })
    }


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
