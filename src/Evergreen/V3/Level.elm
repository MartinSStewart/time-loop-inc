module Evergreen.V3.Level exposing (..)

import AssocSet
import Evergreen.V3.Point


type TileEdge
    = TopEdge
    | BottomEdge
    | LeftEdge
    | RightEdge


type alias Exit =
    { position : Evergreen.V3.Point.Point
    , tileEdge : TileEdge
    }


type alias Portal =
    { position : Evergreen.V3.Point.Point
    , tileEdge : TileEdge
    }


type alias PortalPair =
    { firstPortal : Portal
    , secondPortal : Portal
    , timeDelta : Int
    }


type alias Door =
    { doorPosition : Evergreen.V3.Point.Point
    , buttonPosition : Evergreen.V3.Point.Point
    }


type alias Level_ =
    { playerStart : Evergreen.V3.Point.Point
    , walls : AssocSet.Set Evergreen.V3.Point.Point
    , boxesStart : AssocSet.Set Evergreen.V3.Point.Point
    , exit : Exit
    , levelSize : Evergreen.V3.Point.Point
    , portalPairs : List PortalPair
    , doors : List Door
    }


type Level
    = Level Level_
