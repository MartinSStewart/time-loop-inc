module Level exposing (Level, Portal, PortalPair, TileEdge(..))

import Point exposing (Point)
import Set exposing (Set)


type alias Level =
    { playerStart : Point
    , walls : Set Point
    , boxesStart : Set Point
    , exit : Point
    , levelSize : Point
    , portalPairs : List PortalPair
    }


type alias Portal =
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
