module Editor exposing
    ( Level
    , Model
    , Msg
    , ToBackend(..)
    , ToFrontend(..)
    , animationFrame
    , init
    , initWithLevel
    , initWithReplay
    , keyUpdate
    , update
    , updateFromBackend
    , view
    )

import AssocList as Dict exposing (Dict)
import AssocSet as Set exposing (Set)
import Effect.Browser.Navigation
import Effect.Command as Command exposing (Command, FrontendOnly)
import Effect.Lamdera as Lamdera
import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import Env
import Game exposing (Game, Replay)
import Html.Attributes
import Html.Events.Extra.Pointer
import Id exposing (Id)
import KeyHelper
import Keyboard exposing (Key)
import Level exposing (Door, Exit, Laser, Portal, PortalPair, TileEdge(..), WallType(..))
import LevelState exposing (Direction, LaserBeam)
import List.Extra as List
import List.Nonempty
import Point exposing (Point)
import Route exposing (LevelId, ReplayId)


type Msg
    = PressedSelectTool Tool
    | PressedReset
    | PointerMoved Html.Events.Extra.Pointer.Event
    | PointerDown Html.Events.Extra.Pointer.Event
    | PointerUp Html.Events.Extra.Pointer.Event
    | PressedPlay
    | PressedSave
    | GameMsg Game.Msg
    | PressedBackToEditor
    | PressedSaveRelayAndGoBackToEditor
    | TypedTimeDelta { portalPairIndex : Int, timeDelta : Int }


type Tool
    = WallTool
    | GlassTool
    | PlayerTool
    | BoxTool
    | EraseTool
    | LaserTool
    | PortalTool (Maybe Portal)
    | DoorTool (Maybe Point)
    | ExitTool


type ToBackend
    = SaveLevelRequest Level
    | SaveReplayRequest (Id LevelId) Replay
    | SaveLevelAndReplayRequest Level Replay


type ToFrontend
    = SaveLevelResponse (Id LevelId)
    | SaveReplayResponse (Id LevelId) (Result () (Id ReplayId))
    | SaveLevelAndReplayResponse (Id LevelId) (Id ReplayId)


type alias Model =
    { tool : Tool
    , pointerPosition : Maybe ( Float, Float )
    , pointerIsDown : Bool
    , level : LevelAndId
    , undoHistory : List LevelAndId
    , redoHistory : List LevelAndId
    , game : StartGame
    , isSaving : Bool
    }


type alias LevelAndId =
    { saveStatus : SaveStatus, level : Level }


type SaveStatus
    = NotSaved
    | LevelSaved (Id LevelId)
    | ReplaySaved (Id LevelId) (Id ReplayId)


type StartGame
    = NotStarted { pressedStart : Bool }
    | Started { game : Game, replay : Maybe Replay }


type alias Level =
    { playerStart : Maybe Point
    , walls : Dict Point WallType
    , boxesStart : Set Point
    , exit : Maybe Exit
    , levelSize : Point
    , portalPairs : List PortalPair
    , doors : List Door
    , lasers : List Laser
    }


init : Model
init =
    { tool = WallTool
    , pointerPosition = Nothing
    , pointerIsDown = False
    , level = { saveStatus = NotSaved, level = defaultLevel }
    , undoHistory = []
    , redoHistory = []
    , game = NotStarted { pressedStart = False }
    , isSaving = False
    }


initWithLevel : Id LevelId -> Level -> Model
initWithLevel levelId level =
    { init | level = { saveStatus = LevelSaved levelId, level = level } }


initWithReplay : Id LevelId -> Id ReplayId -> Level -> Replay -> Model
initWithReplay levelId replayId level replay =
    { init
        | level = { saveStatus = ReplaySaved levelId replayId, level = level }
        , game =
            case validateLevel level of
                Ok ok ->
                    { game = Game.init (List.Nonempty.fromElement ok) |> Game.setMoveActions replay.moveActions
                    , replay = Just replay
                    }
                        |> Started

                Err _ ->
                    NotStarted { pressedStart = True }
    }


defaultLevel : Level
defaultLevel =
    { playerStart = Nothing
    , walls = Dict.empty
    , boxesStart = Set.empty
    , exit = Nothing
    , levelSize = ( 10, 10 )
    , portalPairs = []
    , doors = []
    , lasers = []
    }


update : Msg -> Model -> ( Model, Command FrontendOnly ToBackend Msg )
update msg model =
    case msg of
        PressedSelectTool tool ->
            ( { model | tool = tool }, Command.none )

        PressedReset ->
            ( addUndoStep defaultLevel model, Command.none )

        PointerMoved event ->
            let
                gridPosition =
                    pointToGrid event.pointer.offsetPos

                model2 =
                    { model | pointerPosition = Just event.pointer.offsetPos }
            in
            ( if model2.pointerIsDown && insideLevel gridPosition model2.level.level then
                addUndoStep
                    (handlePointerMove gridPosition model2.tool model2.level.level)
                    model2

              else
                model2
            , Command.none
            )

        PointerDown event ->
            let
                model2 =
                    { model
                        | pointerPosition = Just event.pointer.offsetPos
                        , pointerIsDown = True
                    }

                gridPosition =
                    pointToGrid event.pointer.offsetPos

                ( tool, level ) =
                    handlePointerDown event.pointer.offsetPos model2 model2.level.level
            in
            ( if insideLevel gridPosition model2.level.level then
                addUndoStep level { model2 | tool = tool }

              else
                model2
            , Command.none
            )

        PointerUp event ->
            ( { model | pointerPosition = Just event.pointer.offsetPos, pointerIsDown = False }, Command.none )

        PressedPlay ->
            ( case validateLevel model.level.level of
                Ok ok ->
                    { model
                        | game = { game = Game.init (List.Nonempty.fromElement ok), replay = Nothing } |> Started
                    }

                Err _ ->
                    { model | game = NotStarted { pressedStart = True } }
            , Command.none
            )

        GameMsg gameMsg ->
            ( case model.game of
                Started game ->
                    { model | game = { game | game = Game.update gameMsg game.game } |> Started }

                NotStarted _ ->
                    model
            , Command.none
            )

        PressedBackToEditor ->
            let
                level =
                    model.level
            in
            ( { model
                | game = NotStarted { pressedStart = False }
                , level = { level | saveStatus = NotSaved }
              }
            , Command.none
            )

        PressedSaveRelayAndGoBackToEditor ->
            case model.game of
                Started game ->
                    let
                        replay : Replay
                        replay =
                            Game.getReplay game.game
                    in
                    if game.replay == Just replay then
                        ( model, Command.none )

                    else
                        ( { model | game = NotStarted { pressedStart = False }, isSaving = True }
                        , (case model.level.saveStatus of
                            NotSaved ->
                                SaveLevelAndReplayRequest model.level.level replay

                            LevelSaved levelId ->
                                SaveReplayRequest levelId replay

                            ReplaySaved levelId _ ->
                                SaveReplayRequest levelId replay
                          )
                            |> Lamdera.sendToBackend
                        )

                NotStarted _ ->
                    ( model, Command.none )

        TypedTimeDelta { portalPairIndex, timeDelta } ->
            let
                level =
                    model.level.level
            in
            ( addUndoStep
                { level
                    | portalPairs =
                        List.updateAt
                            portalPairIndex
                            (\portalPair -> { portalPair | timeDelta = timeDelta })
                            level.portalPairs
                }
                model
            , Command.none
            )

        PressedSave ->
            if model.isSaving then
                ( model, Command.none )

            else
                ( { model | isSaving = True }, Lamdera.sendToBackend (SaveLevelRequest model.level.level) )


updateFromBackend : Effect.Browser.Navigation.Key -> ToFrontend -> Model -> ( Model, Command FrontendOnly ToBackend Msg )
updateFromBackend navigationKey msg model =
    case msg of
        SaveLevelResponse levelId ->
            let
                level =
                    model.level
            in
            ( { model | level = { level | saveStatus = LevelSaved levelId }, isSaving = False }
            , Effect.Browser.Navigation.replaceUrl navigationKey (levelUrl levelId)
            )

        SaveReplayResponse levelId maybeReplayId ->
            let
                level =
                    model.level
            in
            case maybeReplayId of
                Ok replayId ->
                    ( { model | level = { level | saveStatus = ReplaySaved levelId replayId }, isSaving = False }
                    , Command.none
                    )

                Err () ->
                    ( model, Command.none )

        SaveLevelAndReplayResponse levelId replayId ->
            let
                level =
                    model.level
            in
            ( { model | level = { level | saveStatus = ReplaySaved levelId replayId }, isSaving = False }
            , Effect.Browser.Navigation.replaceUrl navigationKey (levelUrl levelId)
            )


levelUrl : Id LevelId -> String
levelUrl levelId =
    "/level/" ++ Id.toString levelId


replayUrl : Id LevelId -> Id ReplayId -> String
replayUrl levelId replayId =
    "/level/" ++ Id.toString levelId ++ "/" ++ Id.toString replayId


validateLevel : Level -> Result String Level.Level
validateLevel level =
    case ( level.playerStart, level.exit ) of
        ( Just playerStart, Just exit ) ->
            case
                Level.init
                    { playerStart = playerStart
                    , walls = level.walls
                    , boxesStart = level.boxesStart
                    , exit = exit
                    , levelSize = level.levelSize
                    , portalPairs = level.portalPairs
                    , doors = level.doors
                    , lasers = level.lasers
                    }
            of
                Ok ok ->
                    Ok ok

                Err err ->
                    Err err

        ( Nothing, Just _ ) ->
            Err "Player missing"

        ( Just _, Nothing ) ->
            Err "Level exit missing"

        ( Nothing, Nothing ) ->
            Err "Level exit and player missing"


addUndoStep : Level -> Model -> Model
addUndoStep level model =
    let
        levelData =
            model.level
    in
    { model
        | level = { levelData | level = level }
        , undoHistory = model.level :: model.undoHistory
        , redoHistory = []
    }


keyUpdate : { a | keys : List Key, previousKeys : List Key } -> Model -> Model
keyUpdate keyState model =
    case model.game of
        Started game ->
            { model | game = { game | game = Game.keyUpdate keyState game.game } |> Started }

        NotStarted _ ->
            if KeyHelper.undo keyState then
                case model.undoHistory of
                    head :: rest ->
                        { model | undoHistory = rest, level = head, redoHistory = model.level :: model.redoHistory }

                    [] ->
                        model

            else if KeyHelper.redo keyState then
                case model.redoHistory of
                    head :: rest ->
                        { model | redoHistory = rest, level = head, undoHistory = model.level :: model.undoHistory }

                    [] ->
                        model

            else
                model


animationFrame : Model -> Model
animationFrame model =
    case model.game of
        Started game ->
            { model | game = { game | game = Game.animationFrame game.game } |> Started }

        NotStarted _ ->
            model


insideLevel : Point -> Level -> Bool
insideLevel position level =
    Point.clamp ( 0, 0 ) (Point.add ( -1, -1 ) level.levelSize) position == position


handlePointerDown : ( Float, Float ) -> Model -> Level -> ( Tool, Level )
handlePointerDown pointerPosition model level =
    let
        gridPosition =
            pointToGrid pointerPosition
    in
    case model.tool of
        WallTool ->
            ( WallTool, addWall gridPosition level )

        GlassTool ->
            ( GlassTool, addGlass gridPosition level )

        PlayerTool ->
            ( PlayerTool
            , { level
                | playerStart = Just gridPosition
                , boxesStart = Set.remove gridPosition level.boxesStart
                , walls = Dict.remove gridPosition level.walls
              }
            )

        BoxTool ->
            ( BoxTool
            , { level
                | boxesStart =
                    if Set.member gridPosition level.boxesStart then
                        Set.remove gridPosition level.boxesStart

                    else
                        Set.insert gridPosition level.boxesStart
                , playerStart =
                    if Just gridPosition == level.playerStart then
                        Nothing

                    else
                        level.playerStart
                , walls = Dict.remove gridPosition level.walls
              }
            )

        EraseTool ->
            ( EraseTool, eraseTile gridPosition level )

        LaserTool ->
            let
                laser =
                    { position = gridPosition, tileEdge = pointToTileEdge pointerPosition }
            in
            ( LaserTool, { level | lasers = laser :: List.remove laser level.lasers } )

        PortalTool maybePortal ->
            let
                nextPortal : Portal
                nextPortal =
                    { position = gridPosition
                    , tileEdge = pointToTileEdge pointerPosition
                    }
            in
            case maybePortal of
                Just portal ->
                    ( PortalTool Nothing
                    , { level
                        | portalPairs =
                            { timeDelta = 10, firstPortal = portal, secondPortal = nextPortal } :: level.portalPairs
                      }
                    )

                Nothing ->
                    ( PortalTool (Just nextPortal), level )

        DoorTool maybeDoorPosition ->
            case maybeDoorPosition of
                Just doorPosition ->
                    ( DoorTool Nothing
                    , { level
                        | doors =
                            { doorPosition = doorPosition, buttonPosition = gridPosition } :: level.doors
                      }
                    )

                Nothing ->
                    ( DoorTool (Just gridPosition), level )

        ExitTool ->
            let
                exit =
                    { position = gridPosition, tileEdge = pointToTileEdge pointerPosition }
            in
            ( ExitTool, { level | exit = Just exit } )


eraseTile : Point -> Level -> Level
eraseTile gridPosition level =
    { level
        | walls = Dict.remove gridPosition level.walls
        , playerStart =
            if Just gridPosition == level.playerStart then
                Nothing

            else
                level.playerStart
        , boxesStart = Set.remove gridPosition level.boxesStart
        , lasers = List.filter (.position >> (/=) gridPosition) level.lasers
        , portalPairs =
            List.filter
                (\{ firstPortal, secondPortal } ->
                    firstPortal.position /= gridPosition && secondPortal.position /= gridPosition
                )
                level.portalPairs
        , doors =
            List.filter
                (\{ doorPosition, buttonPosition } -> doorPosition /= gridPosition && buttonPosition /= gridPosition)
                level.doors
        , exit =
            if Just gridPosition == Maybe.map .position level.exit then
                Nothing

            else
                level.exit
    }


addWall : Point -> Level -> Level
addWall gridPosition level =
    eraseTile gridPosition level
        |> (\level_ -> { level_ | walls = Dict.insert gridPosition Wall level_.walls })


addGlass : Point -> Level -> Level
addGlass gridPosition level =
    eraseTile gridPosition level
        |> (\level_ -> { level_ | walls = Dict.insert gridPosition Glass level_.walls })


handlePointerMove : Point -> Tool -> Level -> Level
handlePointerMove gridPosition tool level =
    case tool of
        WallTool ->
            addWall gridPosition level

        GlassTool ->
            addGlass gridPosition level

        PlayerTool ->
            level

        BoxTool ->
            level

        EraseTool ->
            eraseTile gridPosition level

        LaserTool ->
            level

        PortalTool _ ->
            level

        DoorTool _ ->
            level

        ExitTool ->
            level


pointToGrid : ( Float, Float ) -> Point
pointToGrid ( x, y ) =
    ( x / tileSize |> floor, y / tileSize |> floor )


pointToTileEdge : ( Float, Float ) -> TileEdge
pointToTileEdge ( x, y ) =
    let
        xMod =
            modBy tileSize (round x)

        yMod =
            modBy tileSize (round y)

        xMod2 =
            tileSize - xMod |> abs

        yMod2 =
            tileSize - yMod |> abs
    in
    if xMod < xMod2 && xMod < yMod && xMod < yMod2 then
        LeftEdge

    else if xMod2 < xMod && xMod2 < yMod && xMod2 < yMod2 then
        RightEdge

    else if yMod < xMod && yMod < yMod2 && yMod < xMod2 then
        TopEdge

    else
        BottomEdge


view : Model -> Element Msg
view model =
    case model.game of
        Started game ->
            Element.column
                []
                [ Game.view game.game |> Element.map GameMsg
                , Element.row
                    [ Element.spacing 16 ]
                    [ button PressedBackToEditor "Back to editor"
                    , if Just (Game.getReplay game.game) == game.replay then
                        Element.none

                      else
                        button PressedSaveRelayAndGoBackToEditor "Save replay and go back to editor"
                    ]
                ]

        NotStarted { pressedStart } ->
            Element.column
                [ Element.width Element.fill, Element.height Element.fill ]
                [ Element.row
                    [ Element.spacing 8
                    , Element.padding 4
                    , Element.Background.color (Element.rgb 0.9 0.9 0.9)
                    , Element.width Element.fill
                    ]
                    [ button PressedPlay "Play"
                    , case ( pressedStart, validateLevel model.level.level ) of
                        ( True, Err error ) ->
                            Element.el [ Element.Font.color (Element.rgb 1 0 0) ] (Element.text error)

                        _ ->
                            Element.none
                    , verticalLine
                    , button PressedSave
                        (if model.isSaving then
                            "Saving..."

                         else
                            "Save"
                        )
                    , case model.level.saveStatus of
                        LevelSaved levelId ->
                            "Saved to " ++ Env.domain ++ levelUrl levelId |> Element.text

                        ReplaySaved levelId replayId ->
                            "Replay saved to " ++ Env.domain ++ replayUrl levelId replayId |> Element.text

                        NotSaved ->
                            Element.none
                    ]
                , Element.row
                    [ Element.width Element.fill, Element.height Element.fill ]
                    [ toolbarView model
                    , Element.column
                        [ Element.alignTop ]
                        [ levelView model
                        , portalPairsView model
                        ]
                    ]
                ]


portalPairsView : Model -> Element Msg
portalPairsView model =
    if List.isEmpty model.level.level.portalPairs then
        Element.none

    else
        Element.text "Portal pair time deltas"
            :: List.indexedMap
                (\index { timeDelta } ->
                    Element.Input.text
                        [ Element.padding 4, Element.width (Element.px 50), Element.Font.alignRight ]
                        { onChange =
                            \text ->
                                TypedTimeDelta
                                    { portalPairIndex = index
                                    , timeDelta =
                                        if text == "" then
                                            0

                                        else
                                            String.toInt text |> Maybe.withDefault timeDelta
                                    }
                        , label =
                            ("Pair " ++ String.fromInt (index + 1))
                                |> Element.text
                                |> Element.Input.labelLeft []
                        , text = String.fromInt timeDelta
                        , placeholder = Nothing
                        }
                )
                model.level.level.portalPairs
            |> Element.column [ Element.padding 8, Element.spacing 4 ]


toolbarView : Model -> Element Msg
toolbarView model =
    let
        toolButton : Tool -> String -> Element Msg
        toolButton select label =
            Element.Input.button
                [ Element.Background.color
                    (if sameTool model.tool select then
                        Element.rgb 0.8 0.9 1

                     else
                        Element.rgb 0.8 0.8 0.8
                    )
                , Element.Border.width 1
                , Element.padding 4
                , Element.width Element.fill
                ]
                { onPress = Just (PressedSelectTool select)
                , label = Element.el [ Element.centerX ] (Element.text label)
                }
    in
    Element.column
        [ Element.spacing 8
        , Element.padding 4
        , Element.Background.color (Element.rgb 0.9 0.9 0.9)
        , Element.height Element.fill
        , Element.alignTop
        ]
        [ toolButton WallTool "Wall"
        , toolButton GlassTool "Glass"
        , toolButton PlayerTool "Player"
        , toolButton BoxTool "Box"
        , toolButton LaserTool "Laser"
        , toolButton (PortalTool Nothing) "Portals"
        , toolButton (DoorTool Nothing) "Door"
        , toolButton EraseTool "Erase"
        , toolButton ExitTool "Place exit"
        ]


verticalLine =
    Element.el
        [ Element.height Element.fill
        , Element.width (Element.px 1)
        , Element.Background.color (Element.rgb 0 0 0)
        ]
        Element.none


button : msg -> String -> Element msg
button onPress label =
    Element.Input.button
        [ Element.Background.color (Element.rgb 0.8 0.8 0.8)
        , Element.Border.width 1
        , Element.padding 4
        ]
        { onPress = Just onPress
        , label = Element.text label
        }


sameTool : Tool -> Tool -> Bool
sameTool toolA toolB =
    if toolA == toolB then
        True

    else
        case ( toolA, toolB ) of
            ( PortalTool _, PortalTool _ ) ->
                True

            ( DoorTool _, DoorTool _ ) ->
                True

            _ ->
                False


levelView : Model -> Element Msg
levelView model =
    Element.el
        [ Element.width Element.fill
        , Html.Events.Extra.Pointer.onMove PointerMoved |> Element.htmlAttribute
        , Html.Events.Extra.Pointer.onDown PointerDown |> Element.htmlAttribute
        , Html.Events.Extra.Pointer.onUp PointerUp |> Element.htmlAttribute
        ]
        (viewLevel model.level.level)


tileSize : number
tileSize =
    50


viewLevel : Level -> Element msg
viewLevel level =
    let
        ( w, h ) =
            level.levelSize

        walls : Dict Point WallType
        walls =
            level.walls

        portals : List { timeDelta : Int, portal : Portal }
        portals =
            List.concatMap
                (\portalPair ->
                    [ { timeDelta = portalPair.timeDelta, portal = portalPair.firstPortal }
                    , { timeDelta = -portalPair.timeDelta, portal = portalPair.secondPortal }
                    ]
                )
                level.portalPairs

        exit =
            level.exit |> Maybe.withDefault { position = ( -1, -1 ), tileEdge = LeftEdge }

        level_ =
            Level.unsafe
                { playerStart = ( -1, -1 )
                , walls = level.walls
                , boxesStart = level.boxesStart
                , exit = { position = ( -2, -2 ), tileEdge = LeftEdge }
                , levelSize = level.levelSize
                , portalPairs = level.portalPairs
                , doors = level.doors
                , lasers = level.lasers
                }

        laserBeams : Set LaserBeam
        laserBeams =
            LevelState.getLaserBeams
                level_
                (LevelState.timeline level_ [])
                0
    in
    List.range 0 (w - 1)
        |> List.map
            (\x ->
                List.range 0 (h - 1)
                    |> List.map
                        (\y ->
                            let
                                localPortals : List ( Int, TileEdge )
                                localPortals =
                                    List.filterMap
                                        (\portal ->
                                            if portal.portal.position == position then
                                                Just ( portal.timeDelta, portal.portal.tileEdge )

                                            else
                                                Nothing
                                        )
                                        portals

                                position : Point
                                position =
                                    Point.new x y

                                borderWidth : TileEdge -> Int
                                borderWidth tileEdge =
                                    if
                                        List.any (Tuple.second >> (==) tileEdge) localPortals
                                            || (exit.position == position)
                                            && (exit.tileEdge == tileEdge)
                                    then
                                        6

                                    else
                                        0

                                maybeDoor =
                                    case List.find (.doorPosition >> (==) position) level.doors of
                                        Just { buttonPosition } ->
                                            (level.playerStart == Just buttonPosition)
                                                || Set.member buttonPosition level.boxesStart
                                                |> Just

                                        Nothing ->
                                            Nothing

                                timePortalText =
                                    List.map
                                        (\( timeDelta, _ ) ->
                                            (if timeDelta < 0 then
                                                "t" ++ String.fromInt timeDelta

                                             else
                                                "t+" ++ String.fromInt timeDelta
                                            )
                                                |> Element.text
                                                |> Element.el
                                                    [ Element.Font.size 12
                                                    , Element.centerX
                                                    , Element.centerY
                                                    ]
                                                |> Element.inFront
                                        )
                                        localPortals
                            in
                            Element.el
                                (Element.width (Element.px tileSize)
                                    :: Element.height (Element.px tileSize)
                                    :: Element.Font.center
                                    :: Element.Border.width 1
                                    :: (Element.el
                                            [ Element.Border.widthEach
                                                { left = borderWidth LeftEdge
                                                , right = borderWidth RightEdge
                                                , top = borderWidth TopEdge
                                                , bottom = borderWidth BottomEdge
                                                }
                                            , Element.Border.color
                                                (if exit.position == position then
                                                    Element.rgb 0 0.8 0

                                                 else
                                                    Element.rgb 0.3 0.3 1
                                                )
                                            , Element.width Element.fill
                                            , Element.height Element.fill
                                            , noPointerEvents
                                            ]
                                            Element.none
                                            |> Element.inFront
                                       )
                                    :: drawWallsAndDoorBackground position maybeDoor walls
                                    :: drawLaser position level.lasers
                                    ++ drawLaserBeam position laserBeams
                                    ++ timePortalText
                                )
                                (if Set.member position level.boxesStart then
                                    Element.el
                                        [ Element.centerX
                                        , Element.centerY
                                        , Element.Font.size 30
                                        ]
                                        (Element.text "▨")

                                 else if Just position == level.playerStart then
                                    Element.el
                                        [ Element.Font.size 24
                                        , Element.Font.bold
                                        , Element.centerX
                                        , Element.centerY
                                        ]
                                        (Element.text "P")

                                 else if level.exit |> Maybe.map .position |> (==) (Just position) then
                                    Element.el
                                        [ Element.centerX
                                        , Element.centerY
                                        , Element.Font.size 14
                                        ]
                                        (Element.text "Exit")

                                 else if level.doors |> List.any (.buttonPosition >> (==) (Point.new x y)) then
                                    Element.el
                                        [ Element.centerX
                                        , Element.centerY
                                        , Element.Font.size 14
                                        ]
                                        (Element.text "B")

                                 else
                                    Element.none
                                )
                        )
                    |> Element.column []
            )
        |> Element.row [ noPointerEvents ]


noPointerEvents =
    Html.Attributes.style "pointer-events" "none" |> Element.htmlAttribute


drawWallsAndDoorBackground position maybeDoor walls =
    case Dict.get position walls of
        Just Wall ->
            Element.Background.color (Element.rgb 0 0 0)

        Just Glass ->
            Element.Background.color (Element.rgb 0.9 0.9 0.8)

        Nothing ->
            case maybeDoor of
                Just True ->
                    Element.Background.color (Element.rgb 0.8 0.8 0.8)

                Just False ->
                    Element.Background.color (Element.rgb 0.4 0.4 0.4)

                Nothing ->
                    Element.Background.color (Element.rgb 1 1 1)


drawLaserBeam : Point -> Set LaserBeam -> List (Element.Attribute msg)
drawLaserBeam position lasers =
    [ (if Set.member { position = position, isVertical = True } lasers then
        Element.el
            [ Element.width (Element.px 4)
            , Element.height Element.fill
            , Element.centerX
            , Element.Background.color (Element.rgb 1 0 0)
            , noPointerEvents
            ]
            Element.none

       else
        Element.none
      )
        |> Element.behindContent
    , (if Set.member { position = position, isVertical = False } lasers then
        Element.el
            [ Element.height (Element.px 4)
            , Element.width Element.fill
            , Element.centerY
            , Element.Background.color (Element.rgb 1 0 0)
            , noPointerEvents
            ]
            Element.none

       else
        Element.none
      )
        |> Element.behindContent
    ]


drawLaser : Point -> List Laser -> List (Element.Attribute msg)
drawLaser position lasers =
    List.filterMap
        (\laser ->
            if laser.position == position then
                Element.el
                    (Element.Background.color (Element.rgb 0.5 0.2 0.2)
                        :: Element.width (Element.px 12)
                        :: Element.height (Element.px 12)
                        :: noPointerEvents
                        :: (case laser.tileEdge of
                                LeftEdge ->
                                    [ Element.centerY ]

                                TopEdge ->
                                    [ Element.centerX ]

                                BottomEdge ->
                                    [ Element.centerX, Element.alignBottom ]

                                RightEdge ->
                                    [ Element.centerY, Element.alignRight ]
                           )
                    )
                    Element.none
                    |> Element.inFront
                    |> Just

            else
                Nothing
        )
        lasers
