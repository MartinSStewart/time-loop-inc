module LevelState exposing
    ( LevelInstant
    , MoveAction(..)
    , Timeline
    , timeline
    )

import AssocSet as Set exposing (Set)
import Level exposing (Level, PortalPair)
import List.Extra as List
import List.Nonempty exposing (Nonempty(..))
import Point exposing (Point)


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


moveActionReverse : MoveAction -> MoveAction
moveActionReverse moveAction =
    case moveAction of
        MoveUp ->
            MoveDown

        MoveLeft ->
            MoveRight

        MoveRight ->
            MoveLeft

        MoveDown ->
            MoveUp

        MoveNone ->
            MoveNone


step :
    Level
    -> List MoveAction
    -> LevelInstant
    -> { nextInstant : LevelInstant, playerTimeTravel : List PlayerInstant }
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

        portalPairs : List PortalPair
        portalPairs =
            Level.portalPairs level

        nextInstant =
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
                            let
                                tryMove : MoveAction -> Maybe Point
                                tryMove moveAction =
                                    if
                                        boxIsPushed box moveAction
                                            && not (boxIsPushed box (moveActionReverse moveAction))
                                    then
                                        let
                                            newPosition =
                                                Point.add (actionOffset moveAction) box.position
                                        in
                                        if Level.isWall level newPosition then
                                            Nothing

                                        else
                                            Just newPosition

                                    else
                                        Nothing
                            in
                            { position =
                                [ MoveRight, MoveLeft, MoveUp, MoveDown ]
                                    |> List.filterMap tryMove
                                    |> List.head
                                    |> Maybe.withDefault box.position
                            , age = box.age + 1
                            }
                        )
                        levelInstant.boxes
            }
    in
    { nextInstant = nextInstant, playerTimeTravel = [] }


getPlayerAt : Point -> LevelInstant -> List PlayerInstant
getPlayerAt point levelInstant =
    List.filter (\player -> player.position == point) levelInstant.players


getBoxAt : Point -> LevelInstant -> List BoxInstant
getBoxAt point levelInstant =
    List.filter (\box -> box.position == point) levelInstant.boxes


timeline : Level -> List MoveAction -> Timeline
timeline level moveActions =
    let
        first =
            init level
    in
    Nonempty first (timelineHelper level first moveActions)


timelineHelper : Level -> Nonempty LevelInstant -> List MoveAction -> Nonempty LevelInstant
timelineHelper level previousInstants moveActions =
    let
        newInstant =
            step level moveActions previousInstant
    in
    if List.any (\player -> player.age > List.length moveActions) newInstant.players then
        []

    else
        newInstant :: timelineHelper level newInstant moveActions


type alias Timeline =
    { startTime : Int
    , instants : Nonempty LevelInstant
    }


init : Level -> LevelInstant
init level =
    { players = [ { position = Level.playerStart level, age = 0 } ]
    , boxes =
        Level.boxesStart level
            |> Set.toList
            |> List.map (\position -> { position = position, age = 0 })
    }


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
