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
    let
        getMoveAction : PlayerInstant -> Maybe MoveAction
        getMoveAction player =
            List.getAt player.age moveActions

        boxIsPushed box moveAction =
            getPlayerAt
                (Point.add (actionOffset moveAction |> Point.negate) box.position)
                levelInstant
                |> List.any (\player -> getMoveAction player == Just moveAction)
    in
    { levelInstant
        | players =
            List.map
                (\player ->
                    { position =
                        case getMoveAction player of
                            Just action ->
                                let
                                    newPosition =
                                        Point.add (actionOffset action) player.position
                                in
                                if Level.isWall level newPosition then
                                    player.position

                                else
                                    newPosition

                            Nothing ->
                                player.position
                    , age = player.age + 1
                    }
                )
                levelInstant.players
        , boxes =
            List.map
                (\box ->
                    { position =
                        if boxIsPushed box MoveRight then
                            let
                                newPosition =
                                    Point.add (actionOffset MoveRight) box.position
                            in
                            if Level.isWall level newPosition then
                                box.position

                            else
                                newPosition

                        else
                            box.position
                    , age = box.age + 1
                    }
                )
                levelInstant.boxes
    }


getPlayerAt : Point -> LevelInstant -> List PlayerInstant
getPlayerAt point levelInstant =
    List.filter (\player -> player.position == point) levelInstant.players


getBoxAt : Point -> LevelInstant -> List BoxInstant
getBoxAt point levelInstant =
    List.filter (\box -> box.position == point) levelInstant.boxes


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
