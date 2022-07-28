module Level exposing
    ( Door
    , Exit
    , Laser
    , Level
    , Portal
    , PortalPair
    , TileEdge(..)
    , WallType(..)
    , blocksLasers
    , blocksMovement
    , boxesStart
    , doors
    , exit
    , getWalls
    , init
    , lasers
    , levelSize
    , playerStart
    , portalPairs
    , unsafe
    )

import AssocList as Dict exposing (Dict)
import AssocSet as Set exposing (Set)
import Point exposing (Point)


type Level
    = Level LevelData


type alias LevelData =
    { playerStart : Point
    , walls : Dict Point WallType
    , boxesStart : Set Point
    , exit : Exit
    , levelSize : Point
    , portalPairs : List PortalPair
    , doors : List Door
    , lasers : List Laser
    }


type WallType
    = Wall
    | Glass


type alias Laser =
    { position : Point
    , tileEdge : TileEdge
    }


type alias Door =
    { doorPosition : Point
    , buttonPosition : Point
    }


portalPairs : Level -> List PortalPair
portalPairs (Level level) =
    level.portalPairs


init : LevelData -> Result String Level
init level =
    if boxesValid level then
        Level level |> Ok

    else
        Err "Failed to create level"


unsafe : LevelData -> Level
unsafe levelData =
    Level levelData


lasers : Level -> List Laser
lasers (Level level) =
    level.lasers


exit : Level -> Exit
exit (Level level) =
    level.exit


doors : Level -> List Door
doors (Level level) =
    level.doors


getWalls : Level -> Dict Point WallType
getWalls (Level level) =
    level.walls


blocksMovement : Level -> Point -> Bool
blocksMovement (Level level) position =
    Dict.member position level.walls
        || (Point.clamp
                (Point.new 0 0)
                (Point.add (Point.new -1 -1) level.levelSize)
                position
                /= position
           )


blocksLasers : Level -> Point -> Bool
blocksLasers (Level level) position =
    case Dict.get position level.walls of
        Just Glass ->
            False

        Just Wall ->
            True

        Nothing ->
            Point.clamp
                (Point.new 0 0)
                (Point.add (Point.new -1 -1) level.levelSize)
                position
                /= position


playerStart : Level -> Point
playerStart (Level level) =
    level.playerStart


boxesStart : Level -> Set Point
boxesStart (Level level) =
    level.boxesStart


levelSize : Level -> Point
levelSize (Level level) =
    level.levelSize


boxesValid : LevelData -> Bool
boxesValid level =
    level.boxesStart |> Set.intersect (Dict.keys level.walls |> Set.fromList) |> Set.size |> (==) 0


type alias Portal =
    { position : Point
    , tileEdge : TileEdge
    }


type alias Exit =
    { position : Point
    , tileEdge : TileEdge
    }


type alias PortalPair =
    { firstPortal : Portal
    , secondPortal : Portal
    , timeDelta : Int
    }


type TileEdge
    = TopEdge
    | BottomEdge
    | LeftEdge
    | RightEdge
