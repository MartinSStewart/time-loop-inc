module Evergreen.V16.Level exposing (..)

import AssocList
import AssocSet
import Evergreen.V16.Point


type WallType
    = Wall
    | Glass


type TileEdge
    = TopEdge
    | BottomEdge
    | LeftEdge
    | RightEdge


type alias Exit =
    { position : Evergreen.V16.Point.Point
    , tileEdge : TileEdge
    }


type alias Portal =
    { position : Evergreen.V16.Point.Point
    , tileEdge : TileEdge
    }


type alias PortalPair =
    { firstPortal : Portal
    , secondPortal : Portal
    , timeDelta : Int
    }


type alias Door =
    { doorPosition : Evergreen.V16.Point.Point
    , buttonPosition : Evergreen.V16.Point.Point
    }


type alias Laser =
    { position : Evergreen.V16.Point.Point
    , tileEdge : TileEdge
    }


type alias LevelData =
    { playerStart : Evergreen.V16.Point.Point
    , walls : AssocList.Dict Evergreen.V16.Point.Point WallType
    , boxesStart : AssocSet.Set Evergreen.V16.Point.Point
    , exit : Exit
    , levelSize : Evergreen.V16.Point.Point
    , portalPairs : List PortalPair
    , doors : List Door
    , lasers : List Laser
    }


type Level
    = Level LevelData
