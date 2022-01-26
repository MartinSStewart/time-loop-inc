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


step :
    Level
    -> List (Maybe MoveAction)
    -> List PlayerInstant
    -> LevelInstant
    -> { nextInstant : LevelInstant, playerTimeTravel : List ( Int, PlayerInstant ) }
step level moveActions timeTravellers levelInstant =
    let
        getMoveAction : PlayerInstant -> Maybe MoveAction
        getMoveAction player =
            List.getAt player.age moveActions |> Maybe.andThen identity

        boxIsPushed : BoxInstant -> Maybe MoveAction -> Bool
        boxIsPushed box moveAction =
            getPlayerAt
                (Point.add (actionOffset moveAction |> Point.negate) box.position)
                levelInstant
                |> List.any (\player -> getMoveAction player == moveAction)

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
                        ++ timeTravellers
                , boxes =
                    List.map
                        (\box ->
                            let
                                tryMove : MoveAction -> Maybe Point
                                tryMove moveAction =
                                    if
                                        boxIsPushed box (Just moveAction)
                                            && not (boxIsPushed box (Just (moveActionReverse moveAction)))
                                    then
                                        let
                                            newPosition =
                                                Point.add (actionOffset (Just moveAction)) box.position
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
                            }
                        )
                        levelInstant.boxes
            }
    in
    { nextInstant = nextInstant
    , playerTimeTravel =
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
                        )
                            |> Just

                    Nothing ->
                        Nothing
            )
            levelInstant.players
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
            Debug.todo "This shouldn't happen"


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
                                    , resolvable = False
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
    , resolvable : Bool
    }


getPlayerAt : Point -> LevelInstant -> List PlayerInstant
getPlayerAt point levelInstant =
    List.filter (\player -> player.position == point) levelInstant.players


getBoxAt : Point -> LevelInstant -> List BoxInstant
getBoxAt point levelInstant =
    List.filter (\box -> box.position == point) levelInstant.boxes


timeline : Level -> List (Maybe MoveAction) -> RegularDict.Dict Int LevelInstant
timeline level moveActions =
    timelineHelper level (RegularDict.singleton 0 (init 0 level)) [] 0 moveActions


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


timelineHelper :
    Level
    -> RegularDict.Dict Int LevelInstant
    -> List { appearTime : Int, player : PlayerInstant }
    -> Int
    -> List (Maybe MoveAction)
    -> RegularDict.Dict Int LevelInstant
timelineHelper level timeline_ futurePlayers currentTime moveActions =
    let
        --_ =
        --    Debug.log "" ( currentTime, timeline_ )
        { nextInstant, playerTimeTravel } =
            case RegularDict.get currentTime timeline_ of
                Just currentInstant ->
                    step
                        level
                        moveActions
                        (List.filterMap
                            (\{ appearTime, player } ->
                                if appearTime == currentTime + 1 then
                                    Just player

                                else
                                    Nothing
                            )
                            futurePlayers
                        )
                        currentInstant

                Nothing ->
                    --Debug.todo "Failed to get instant"
                    { nextInstant = { boxes = [], players = [] }, playerTimeTravel = [] }
    in
    case playerTimeTravel of
        ( timeDelta, player ) :: _ ->
            let
                newTime =
                    currentTime + timeDelta
            in
            if timeDelta > 0 then
                timelineHelper
                    level
                    (RegularDict.insert (currentTime + 1) nextInstant timeline_)
                    ({ appearTime = newTime, player = player } :: futurePlayers)
                    (currentTime + 1)
                    moveActions

            else
                case RegularDict.get newTime timeline_ of
                    Just timeTravelInstant ->
                        if List.any ((==) player) timeTravelInstant.players then
                            --let
                            --    _ =
                            --        Debug.log "stable instant" ""
                            --in
                            timelineHelper
                                level
                                (RegularDict.insert (currentTime + 1) nextInstant timeline_)
                                futurePlayers
                                (currentTime + 1)
                                moveActions

                        else
                            --let
                            --    _ =
                            --        Debug.log "existing instant" ""
                            --in
                            timelineHelper
                                level
                                (RegularDict.insert
                                    newTime
                                    { timeTravelInstant | players = player :: timeTravelInstant.players }
                                    timeline_
                                )
                                futurePlayers
                                newTime
                                moveActions

                    Nothing ->
                        let
                            --_ =
                            --    Debug.log "new instant" ""
                            timeTravelInstant =
                                init newTime level
                        in
                        timelineHelper
                            level
                            (RegularDict.insert
                                newTime
                                { timeTravelInstant | players = player :: timeTravelInstant.players }
                                timeline_
                            )
                            futurePlayers
                            newTime
                            moveActions

        [] ->
            if isTimelineFinished timeline_ moveActions currentTime then
                timeline_

            else
                timelineHelper
                    level
                    (RegularDict.insert (currentTime + 1) nextInstant timeline_)
                    futurePlayers
                    (currentTime + 1)
                    moveActions


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
