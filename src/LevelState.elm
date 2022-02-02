module LevelState exposing
    ( DoorInstant
    , LevelInstant
    , MoveAction(..)
    , Paradox
    , canMakeMove
    , currentPlayerTime
    , doors
    , getTimelineInstant
    , hasParadoxes
    , isCompleted
    , paradoxes
    , timeline
    )

import AssocSet as Set exposing (Set)
import Dict as RegularDict
import Level exposing (Door, Level, Portal, PortalPair, TileEdge(..))
import List.Extra as List
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
    { position : Point }


type MoveAction
    = MoveUp
    | MoveLeft
    | MoveRight
    | MoveDown


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


type NewBox
    = NormalBox BoxInstant
    | TimeTravelBox ( Int, BoxInstant )


step :
    Level
    -> RegularDict.Dict Int LevelInstant
    -> Int
    -> List (Maybe MoveAction)
    -> List BoxOrPlayer
    -> LevelInstant
    ->
        { nextInstant : LevelInstant
        , timeTravelers : List ( Int, BoxOrPlayer )
        }
step level timeline_ currentTime moveActions timeTravellers levelInstant =
    let
        getMoveAction : PlayerInstant -> Maybe MoveAction
        getMoveAction player =
            List.getAt player.age moveActions |> Maybe.andThen identity

        boxIsPushed : BoxInstant -> MoveAction -> Bool
        boxIsPushed box moveAction =
            let
                reverseMoveTileEdge =
                    moveActionReverse moveAction |> Just
            in
            case
                List.find
                    (\{ portal } ->
                        portal.position == box.position && movesIntoTileEdge reverseMoveTileEdge portal.tileEdge
                    )
                    portals
            of
                Just { timeDelta, portalExit } ->
                    case RegularDict.get (timeDelta + currentTime) timeline_ of
                        Just instant ->
                            getPlayerAt portalExit.position instant
                                |> List.any
                                    (\player ->
                                        movesIntoTileEdge (getMoveAction player) portalExit.tileEdge
                                    )

                        Nothing ->
                            False

                Nothing ->
                    getPlayerAt
                        (Point.add (actionOffset (Just moveAction) |> Point.negate) box.position)
                        levelInstant
                        |> List.any (\player -> getMoveAction player == Just moveAction)

        portalPairs : List PortalPair
        portalPairs =
            Level.portalPairs level

        portals : List { timeDelta : Int, portal : Portal, portalExit : Portal }
        portals =
            List.concatMap
                (\portalPair ->
                    [ { timeDelta = portalPair.timeDelta
                      , portal = portalPair.firstPortal
                      , portalExit = portalPair.secondPortal
                      }
                    , { timeDelta = -portalPair.timeDelta
                      , portal = portalPair.secondPortal
                      , portalExit = portalPair.firstPortal
                      }
                    ]
                )
                portalPairs

        boxes : List NewBox
        boxes =
            List.map
                (\box ->
                    let
                        tryMove : MoveAction -> Maybe NewBox
                        tryMove moveAction =
                            if
                                boxIsPushed box moveAction
                                    && not (boxIsPushed box (moveActionReverse moveAction))
                            then
                                case List.find (.portal >> entersPortal box.position (Just moveAction)) portals of
                                    Just portal ->
                                        TimeTravelBox ( portal.timeDelta, { position = portal.portalExit.position } )
                                            |> Just

                                    Nothing ->
                                        let
                                            newPosition =
                                                Point.add (actionOffset (Just moveAction)) box.position
                                        in
                                        if Level.isWall level newPosition then
                                            Nothing

                                        else
                                            NormalBox { position = newPosition } |> Just

                            else
                                Nothing
                    in
                    case List.filterMap tryMove [ MoveRight, MoveLeft, MoveUp, MoveDown ] of
                        newBox :: _ ->
                            newBox

                        [] ->
                            NormalBox box
                )
                levelInstant.boxes

        nextInstant : LevelInstant
        nextInstant =
            { levelInstant
                | players =
                    List.filterMap
                        (\player ->
                            let
                                moveAction : Maybe MoveAction
                                moveAction =
                                    getMoveAction player
                            in
                            if List.any (.portal >> entersPortal player.position moveAction) portals then
                                Nothing

                            else
                                { position =
                                    let
                                        newPosition =
                                            Point.add (actionOffset moveAction) player.position
                                    in
                                    if Level.isWall level newPosition then
                                        player.position

                                    else
                                        newPosition
                                , age = player.age + 1
                                }
                                    |> Just
                        )
                        levelInstant.players
                        ++ List.filterMap
                            (\timeTraveller ->
                                case timeTraveller of
                                    Player player ->
                                        Just player

                                    Box _ ->
                                        Nothing
                            )
                            timeTravellers
                , boxes =
                    List.filterMap
                        (\newBox ->
                            case newBox of
                                NormalBox normalBox ->
                                    Just normalBox

                                TimeTravelBox _ ->
                                    Nothing
                        )
                        boxes
                        ++ List.filterMap
                            (\timeTraveller ->
                                case timeTraveller of
                                    Player _ ->
                                        Nothing

                                    Box box ->
                                        Just box
                            )
                            timeTravellers
            }
    in
    { nextInstant = nextInstant
    , timeTravelers =
        List.filterMap
            (\player ->
                let
                    moveAction : Maybe MoveAction
                    moveAction =
                        getMoveAction player
                in
                case List.filter (.portal >> entersPortal player.position moveAction) portals |> List.head of
                    Just portal ->
                        ( portal.timeDelta
                        , { position = portal.portalExit.position
                          , age = player.age + 1
                          }
                            |> Player
                        )
                            |> Just

                    Nothing ->
                        Nothing
            )
            levelInstant.players
            ++ List.filterMap
                (\newBox ->
                    case newBox of
                        NormalBox _ ->
                            Nothing

                        TimeTravelBox ( timeDelta, box ) ->
                            Just ( timeDelta, Box box )
                )
                boxes
    }


isCompleted : Level -> RegularDict.Dict Int LevelInstant -> List (Maybe MoveAction) -> Bool
isCompleted level timeline_ moveActions =
    case paradoxes level timeline_ of
        [] ->
            let
                currentTime =
                    currentPlayerTime timeline_ moveActions

                currentPlayer =
                    getTimelineInstant level currentTime timeline_
                        |> .players
                        |> List.find (\player -> player.age == List.length moveActions)

                exit =
                    Level.exit level
            in
            case currentPlayer of
                Just currentPlayer_ ->
                    currentPlayer_.position == exit.position

                Nothing ->
                    False

        _ ->
            False



--case paradoxes level timeline_ of
--    [] ->
--        let
--            currentTime =
--                currentPlayerTime timeline_ moveActions
--
--            currentPlayer =
--                getTimelineInstant level currentTime timeline_
--                    |> .players
--                    |> List.find (\player -> player.age == List.length moveActions)
--
--            exit =
--                Level.exit level
--        in
--        case currentPlayer of
--            Just player ->
--                (player.position == exit.position)
--                    && movesIntoTileEdge nextMove exit.tileEdge
--                    && hasParadoxes level timeline_
--                    |> not
--
--            Nothing ->
--                Debug.todo "This shouldn't happen"
--
--
--    _ ->
--        False


hasParadoxes : Level -> RegularDict.Dict Int LevelInstant -> Bool
hasParadoxes level timeline_ =
    paradoxes level timeline_ |> List.isEmpty |> not


canMakeMove : Level -> RegularDict.Dict Int LevelInstant -> List (Maybe MoveAction) -> Maybe MoveAction -> Bool
canMakeMove level timeline_ moveActions nextMove =
    let
        currentTime : Int
        currentTime =
            currentPlayerTime timeline_ moveActions

        currentPlayer =
            getTimelineInstant level currentTime timeline_
                |> .players
                |> List.find (\player -> player.age == List.length moveActions)

        exit =
            Level.exit level
    in
    case currentPlayer of
        Just player ->
            (player.position == exit.position)
                && movesIntoTileEdge nextMove exit.tileEdge
                && hasParadoxes level timeline_
                |> not

        Nothing ->
            --Debug.todo "This shouldn't happen"
            True


currentPlayerTime : RegularDict.Dict Int LevelInstant -> List (Maybe MoveAction) -> Int
currentPlayerTime timeline_ moveActions =
    List.find
        (\( _, instant ) ->
            List.any
                (\player -> player.age == List.length moveActions)
                instant.players
        )
        (RegularDict.toList timeline_)
        |> Maybe.map Tuple.first
        |> Maybe.withDefault 0


paradoxes : Level -> RegularDict.Dict Int LevelInstant -> List Paradox
paradoxes level timeline_ =
    RegularDict.toList timeline_
        |> List.concatMap
            (\( time, instant ) ->
                List.map .position instant.players
                    ++ List.map .position instant.boxes
                    ++ List.filterMap
                        (\{ door, isOpen } ->
                            if isOpen then
                                Nothing

                            else
                                Just door.doorPosition
                        )
                        (doors level instant)
                    |> List.gatherEquals
                    |> List.filterMap
                        (\( position, rest ) ->
                            case rest of
                                [] ->
                                    Nothing

                                _ ->
                                    { time = time
                                    , position = position
                                    }
                                        |> Just
                        )
            )


type alias DoorInstant =
    { door : Door
    , isOpen : Bool
    }


doors : Level -> LevelInstant -> List DoorInstant
doors level instant =
    let
        positions =
            List.map .position instant.players
                ++ List.map .position instant.boxes
                |> Set.fromList
    in
    Level.doors level
        |> List.map (\door -> { door = door, isOpen = Set.member door.buttonPosition positions })


type alias Paradox =
    { time : Int
    , position : Point
    }


getPlayerAt : Point -> LevelInstant -> List PlayerInstant
getPlayerAt point levelInstant =
    List.filter (\player -> player.position == point) levelInstant.players


getBoxAt : Point -> LevelInstant -> List BoxInstant
getBoxAt point levelInstant =
    List.filter (\box -> box.position == point) levelInstant.boxes


timeline : Level -> List (Maybe MoveAction) -> RegularDict.Dict Int LevelInstant
timeline level moveActions =
    timelineHelper level (RegularDict.singleton 0 (init 0 level)) Set.empty 0 PlayerTimeTravel moveActions


getTimelineInstant : Level -> Int -> RegularDict.Dict Int LevelInstant -> LevelInstant
getTimelineInstant level currentTime timeline_ =
    let
        latestInstant =
            RegularDict.keys timeline_ |> List.maximum |> Maybe.withDefault 0

        earliestInstant =
            RegularDict.keys timeline_ |> List.minimum |> Maybe.withDefault 0
    in
    if currentTime < earliestInstant then
        init currentTime level

    else if currentTime > latestInstant then
        RegularDict.get (min latestInstant currentTime) timeline_
            |> Maybe.withDefault { players = [], boxes = [] }
            |> (\instant ->
                    { instant
                        | players =
                            List.map
                                (\player -> { player | age = player.age + currentTime - latestInstant })
                                instant.players
                    }
               )

    else
        RegularDict.get (min latestInstant currentTime) timeline_
            |> Maybe.withDefault { players = [], boxes = [] }


type BoxOrPlayer
    = Box BoxInstant
    | Player PlayerInstant


newTime timeDelta currentTime =
    currentTime + 1 + timeDelta


type Mode
    = PlayerTimeTravel
    | BoxTimeTravel


timelineHelper :
    Level
    -> RegularDict.Dict Int LevelInstant
    -> Set { appearTime : Int, item : BoxOrPlayer }
    -> Int
    -> Mode
    -> List (Maybe MoveAction)
    -> RegularDict.Dict Int LevelInstant
timelineHelper level timeline_ futurePlayers currentTime mode moveActions =
    let
        { nextInstant, timeTravelers } =
            case RegularDict.get currentTime timeline_ of
                Just currentInstant ->
                    step
                        level
                        timeline_
                        currentTime
                        moveActions
                        (List.filterMap
                            (\{ appearTime, item } ->
                                if appearTime == currentTime + 1 then
                                    Just item

                                else
                                    Nothing
                            )
                            (Set.toList futurePlayers)
                        )
                        currentInstant

                Nothing ->
                    --Debug.todo "Failed to get instant"
                    { nextInstant = { boxes = [], players = [] }, timeTravelers = [] }
    in
    case
        List.filter
            (\timeTraveller ->
                if isNewTimeTravel timeline_ currentTime timeTraveller then
                    case ( mode, Tuple.second timeTraveller ) of
                        ( PlayerTimeTravel, Box _ ) ->
                            False

                        _ ->
                            True

                else
                    False
            )
            timeTravelers
    of
        ( timeDelta, item ) :: _ ->
            let
                newTime_ =
                    newTime timeDelta currentTime

                timeTravelInstant =
                    case RegularDict.get newTime_ timeline_ of
                        Just timeTravelInstant_ ->
                            timeTravelInstant_

                        Nothing ->
                            init newTime_ level
            in
            if timeDelta > 0 then
                timelineHelper
                    level
                    (RegularDict.insert (currentTime + 1) nextInstant timeline_)
                    (Set.insert { appearTime = newTime_, item = item } futurePlayers)
                    (currentTime + 1)
                    mode
                    moveActions

            else
                timelineHelper
                    level
                    (RegularDict.insert
                        newTime_
                        { timeTravelInstant
                            | players =
                                case item of
                                    Player player ->
                                        player :: timeTravelInstant.players

                                    Box _ ->
                                        timeTravelInstant.players
                            , boxes =
                                case item of
                                    Player _ ->
                                        timeTravelInstant.boxes

                                    Box box ->
                                        box :: timeTravelInstant.boxes
                        }
                        timeline_
                    )
                    futurePlayers
                    newTime_
                    mode
                    moveActions

        [] ->
            if isTimelineFinished timeline_ moveActions currentTime then
                case mode of
                    PlayerTimeTravel ->
                        timelineHelper
                            level
                            timeline_
                            futurePlayers
                            0
                            BoxTimeTravel
                            moveActions

                    BoxTimeTravel ->
                        timeline_

            else
                timelineHelper
                    level
                    (RegularDict.insert (currentTime + 1) nextInstant timeline_)
                    futurePlayers
                    (currentTime + 1)
                    mode
                    moveActions


isNewTimeTravel : RegularDict.Dict Int LevelInstant -> Int -> ( Int, BoxOrPlayer ) -> Bool
isNewTimeTravel timeline_ currentTime ( timeDelta, item ) =
    case RegularDict.get (newTime timeDelta currentTime) timeline_ of
        Just timeTravelInstant ->
            List.any
                ((==) item)
                (List.map Player timeTravelInstant.players ++ List.map Box timeTravelInstant.boxes)
                |> not

        Nothing ->
            True


isTimelineFinished : RegularDict.Dict Int LevelInstant -> List (Maybe MoveAction) -> Int -> Bool
isTimelineFinished timeline_ moveActions currentTime =
    let
        timelineList =
            RegularDict.toList timeline_

        latestTime =
            List.maximumBy Tuple.first timelineList |> Maybe.map Tuple.first |> Maybe.withDefault 0

        totalMoves =
            List.length moveActions
    in
    List.any (\( _, instant ) -> List.any (.age >> (==) totalMoves) instant.players) timelineList
        && currentTime
        >= latestTime


init : Int -> Level -> LevelInstant
init currentTime level =
    { players = [ { position = Level.playerStart level, age = currentTime } ]
    , boxes =
        Level.boxesStart level
            |> Set.toList
            |> List.map (\position -> { position = position })
    }


actionOffset : Maybe MoveAction -> Point
actionOffset action =
    case action of
        Just MoveUp ->
            ( 0, -1 )

        Just MoveLeft ->
            ( -1, 0 )

        Just MoveRight ->
            ( 1, 0 )

        Just MoveDown ->
            ( 0, 1 )

        Nothing ->
            ( 0, 0 )


entersPortal : Point -> Maybe MoveAction -> Portal -> Bool
entersPortal position moveAction portal =
    if position == portal.position then
        movesIntoTileEdge moveAction portal.tileEdge

    else
        False


movesIntoTileEdge : Maybe MoveAction -> TileEdge -> Bool
movesIntoTileEdge moveAction tileEdge =
    case ( moveAction, tileEdge ) of
        ( Just MoveUp, TopEdge ) ->
            True

        ( Just MoveLeft, LeftEdge ) ->
            True

        ( Just MoveRight, RightEdge ) ->
            True

        ( Just MoveDown, BottomEdge ) ->
            True

        _ ->
            False
