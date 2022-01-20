module Level exposing (Level, Portal, PortalPair, TileEdge(..), boxesStart, getWalls, init, isWall, levelSize, playerStart, portalPairs)

import AssocSet as Set exposing (Set)
import Point exposing (Point)


type Level
    = Level Level_


type alias Level_ =
    { playerStart : Point
    , walls : Set Point
    , boxesStart : Set Point
    , exit : Exit
    , levelSize : Point
    , portalPairs : List PortalPair
    }


portalPairs : Level -> List PortalPair
portalPairs (Level level) =
    level.portalPairs


init : Level_ -> Result String Level
init level =
    if boxesValid level then
        Level level |> Ok

    else
        Err "Failed to create level"


getWalls : Level -> Set Point
getWalls (Level level) =
    level.walls


isWall : Level -> Point -> Bool
isWall (Level level) position =
    Set.member position level.walls
        || (Point.clamp
                (Point.new 0 0)
                (Point.add (Point.new -1 -1) level.levelSize)
                position
                /= position
           )


playerStart : Level -> Point
playerStart (Level level) =
    level.playerStart


boxesStart : Level -> Set Point
boxesStart (Level level) =
    level.boxesStart


levelSize : Level -> Point
levelSize (Level level) =
    level.levelSize


boxesValid : Level_ -> Bool
boxesValid level =
    level.boxesStart |> Set.intersect level.walls |> Set.size |> (==) 0


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
