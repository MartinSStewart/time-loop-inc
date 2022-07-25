module Evergreen.V15.Level exposing (..)

import AssocList
import AssocSet
import Evergreen.V15.Point


type WallType
    = Wall
    | Glass


type TileEdge
    = TopEdge
    | BottomEdge
    | LeftEdge
    | RightEdge


type alias Exit =
    { position : Evergreen.V15.Point.Point
    , tileEdge : TileEdge
    }


type alias Portal =
    { position : Evergreen.V15.Point.Point
    , tileEdge : TileEdge
    }


type alias PortalPair =
    { firstPortal : Portal
    , secondPortal : Portal
    , timeDelta : Int
    }


type alias Door =
    { doorPosition : Evergreen.V15.Point.Point
    , buttonPosition : Evergreen.V15.Point.Point
    }


type alias Laser =
    { position : Evergreen.V15.Point.Point
    , tileEdge : TileEdge
    }


type alias LevelData =
    { playerStart : Evergreen.V15.Point.Point
    , walls : AssocList.Dict Evergreen.V15.Point.Point WallType
    , boxesStart : AssocSet.Set Evergreen.V15.Point.Point
    , exit : Exit
    , levelSize : Evergreen.V15.Point.Point
    , portalPairs : List PortalPair
    , doors : List Door
    , lasers : List Laser
    }


type Level
    = Level LevelData
