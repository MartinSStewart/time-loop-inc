module Evergreen.V9.Level exposing (..)

import AssocSet
import Evergreen.V9.Point


type TileEdge
    = TopEdge
    | BottomEdge
    | LeftEdge
    | RightEdge


type alias Exit =
    { position : Evergreen.V9.Point.Point
    , tileEdge : TileEdge
    }


type alias Portal =
    { position : Evergreen.V9.Point.Point
    , tileEdge : TileEdge
    }


type alias PortalPair =
    { firstPortal : Portal
    , secondPortal : Portal
    , timeDelta : Int
    }


type alias Door =
    { doorPosition : Evergreen.V9.Point.Point
    , buttonPosition : Evergreen.V9.Point.Point
    }


type alias Level_ =
    { playerStart : Evergreen.V9.Point.Point
    , walls : AssocSet.Set Evergreen.V9.Point.Point
    , boxesStart : AssocSet.Set Evergreen.V9.Point.Point
    , exit : Exit
    , levelSize : Evergreen.V9.Point.Point
    , portalPairs : List PortalPair
    , doors : List Door
    }


type Level
    = Level Level_
