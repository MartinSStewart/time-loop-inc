module Evergreen.V1.Level exposing (..)

import AssocSet
import Evergreen.V1.Point


type TileEdge
    = TopEdge
    | BottomEdge
    | LeftEdge
    | RightEdge


type alias Exit =
    { position : Evergreen.V1.Point.Point
    , tileEdge : TileEdge
    }


type alias Portal =
    { position : Evergreen.V1.Point.Point
    , tileEdge : TileEdge
    }


type alias PortalPair =
    { firstPortal : Portal
    , secondPortal : Portal
    , timeDelta : Int
    }


type alias Level_ =
    { playerStart : Evergreen.V1.Point.Point
    , walls : AssocSet.Set Evergreen.V1.Point.Point
    , boxesStart : AssocSet.Set Evergreen.V1.Point.Point
    , exit : Exit
    , levelSize : Evergreen.V1.Point.Point
    , portalPairs : List PortalPair
    }


type Level
    = Level Level_
