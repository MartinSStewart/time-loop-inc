module Level exposing (Level, Portal, PortalPair, TileEdge(..), init)

import Point exposing (Point)
import Set exposing (Set)


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


init : Point -> Point -> Exit -> Set Point -> List PortalPair -> Set Point -> Result String Level
init levelSize playerStart exit walls portalPairs boxes =
    let
        level =
            { playerStart = playerStart
            , walls = walls
            , boxesStart = boxes
            , exit = exit
            , levelSize = levelSize
            , portalPairs = portalPairs
            }
    in
    if boxesValid level then
        Level level |> Ok

    else
        Err "Failed to create level"


boxesValid : Level_ -> Bool
boxesValid level =
    level.boxesStart |> Set.intersect level.walls |> Set.size |> (>) 0


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
