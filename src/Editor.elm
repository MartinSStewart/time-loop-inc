module Editor exposing (Model, Msg, init, update, view)

import AssocList as Dict exposing (Dict)
import AssocSet as Set exposing (Set)
import Dict as RegularDict
import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import Html.Attributes
import Html.Events.Extra.Pointer
import Level exposing (Door, Exit, Laser, Portal, PortalPair, TileEdge(..), WallType(..))
import LevelState exposing (LaserBeam)
import List.Extra as List
import Point exposing (Point)


type Msg
    = PressedSelectTool Tool
    | PressedReset
    | PointerMoved Html.Events.Extra.Pointer.Event
    | PointerDown Html.Events.Extra.Pointer.Event
    | PointerUp Html.Events.Extra.Pointer.Event


type Tool
    = WallTool
    | GlassTool
    | PlayerTool
    | BoxTool
    | EraseTool
    | LaserTool
    | PortalTool (Maybe Portal)


type alias Model =
    { tool : Tool
    , pointerPosition : Maybe ( Float, Float )
    , pointerIsDown : Bool
    , level : Level
    }


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
    , level = defaultLevel
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


update : Msg -> Model -> Model
update msg model =
    case msg of
        PressedSelectTool tool ->
            { model | tool = tool }

        PressedReset ->
            { model | level = defaultLevel }

        PointerMoved event ->
            let
                gridPosition =
                    pointToGrid event.pointer.offsetPos
            in
            { model
                | level =
                    if model.pointerIsDown && insideLevel gridPosition model.level then
                        handlePointerMove gridPosition model.tool model.level

                    else
                        model.level
                , pointerPosition = Just event.pointer.offsetPos
            }

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
                    handlePointerDown event.pointer.offsetPos model2 model2.level
            in
            if insideLevel gridPosition model2.level then
                { model2 | level = level, tool = tool }

            else
                model2

        PointerUp event ->
            { model | pointerPosition = Just event.pointer.offsetPos, pointerIsDown = False }


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
                | boxesStart = Set.insert gridPosition level.boxesStart
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
                            { timeDelta = 6, firstPortal = portal, secondPortal = nextPortal } :: level.portalPairs
                      }
                    )

                Nothing ->
                    ( PortalTool (Just nextPortal), level )


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
    Element.column
        [ Element.width Element.fill, Element.height Element.fill ]
        [ toolbarView model.tool
        , levelView model
        ]


toolbarView : Tool -> Element Msg
toolbarView tool =
    Element.row
        [ Element.spacing 8
        , Element.padding 4
        , Element.Background.color (Element.rgb 0.9 0.9 0.9)
        , Element.width Element.fill
        ]
        [ toolButton tool WallTool "Wall"
        , toolButton tool GlassTool "Glass"
        , toolButton tool PlayerTool "Player"
        , toolButton tool BoxTool "Box"
        , toolButton tool LaserTool "Laser"
        , toolButton tool (PortalTool Nothing) "Portals"
        , toolButton tool EraseTool "Erase"
        ]


toolButton : Tool -> Tool -> String -> Element Msg
toolButton currentTool tool label =
    Element.Input.button
        [ Element.Background.color
            (if sameTool currentTool tool then
                Element.rgb 0.8 0.9 1

             else
                Element.rgb 0.8 0.8 0.8
            )
        , Element.Border.width 1
        , Element.padding 4
        ]
        { onPress = Just (PressedSelectTool tool)
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

            _ ->
                False


levelView : Model -> Element Msg
levelView model =
    Element.el
        [ Element.width Element.fill
        , Element.height Element.fill
        , Html.Events.Extra.Pointer.onMove PointerMoved |> Element.htmlAttribute
        , Html.Events.Extra.Pointer.onDown PointerDown |> Element.htmlAttribute
        , Html.Events.Extra.Pointer.onUp PointerUp |> Element.htmlAttribute
        ]
        (viewLevel model.level)


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
                                    :: drawWallsAndDoorBackground position walls
                                    :: drawLaser position level.lasers
                                    ++ drawLaserBeam position laserBeams
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

                                 else
                                    Element.none
                                )
                        )
                    |> Element.column []
            )
        |> Element.row [ noPointerEvents ]


noPointerEvents =
    Html.Attributes.style "pointer-events" "none" |> Element.htmlAttribute


drawWallsAndDoorBackground position walls =
    case Dict.get position walls of
        Just Wall ->
            Element.Background.color (Element.rgb 0 0 0)

        Just Glass ->
            Element.Background.color (Element.rgb 0.9 0.9 0.8)

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
