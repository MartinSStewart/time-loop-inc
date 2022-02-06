module Evergreen.V12.Level exposing (..)

import AssocList
import AssocSet
import Evergreen.V12.Point


type WallType
    = Wall
    | Glass


type TileEdge
    = TopEdge
    | BottomEdge
    | LeftEdge
    | RightEdge


type alias Exit =
    { position : Evergreen.V12.Point.Point
    , tileEdge : TileEdge
    }


type alias Portal =
    { position : Evergreen.V12.Point.Point
    , tileEdge : TileEdge
    }


type alias PortalPair =
    { firstPortal : Portal
    , secondPortal : Portal
    , timeDelta : Int
    }


type alias Door =
    { doorPosition : Evergreen.V12.Point.Point
    , buttonPosition : Evergreen.V12.Point.Point
    }


type alias Laser =
    { position : Evergreen.V12.Point.Point
    , tileEdge : TileEdge
    }


type alias Level_ =
    { playerStart : Evergreen.V12.Point.Point
    , walls : AssocList.Dict Evergreen.V12.Point.Point WallType
    , boxesStart : AssocSet.Set Evergreen.V12.Point.Point
    , exit : Exit
    , levelSize : Evergreen.V12.Point.Point
    , portalPairs : List PortalPair
    , doors : List Door
    , lasers : List Laser
    }


type Level
    = Level Level_
