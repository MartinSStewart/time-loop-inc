module Evergreen.V15.Editor exposing (..)

import AssocList
import AssocSet
import Evergreen.V15.Game
import Evergreen.V15.Id
import Evergreen.V15.Level
import Evergreen.V15.Point
import Html.Events.Extra.Pointer


type LevelId
    = LevelId Never


type alias Level =
    { playerStart : Maybe Evergreen.V15.Point.Point
    , walls : AssocList.Dict Evergreen.V15.Point.Point Evergreen.V15.Level.WallType
    , boxesStart : AssocSet.Set Evergreen.V15.Point.Point
    , exit : Maybe Evergreen.V15.Level.Exit
    , levelSize : Evergreen.V15.Point.Point
    , portalPairs : List Evergreen.V15.Level.PortalPair
    , doors : List Evergreen.V15.Level.Door
    , lasers : List Evergreen.V15.Level.Laser
    }


type Tool
    = WallTool
    | GlassTool
    | PlayerTool
    | BoxTool
    | EraseTool
    | LaserTool
    | PortalTool (Maybe Evergreen.V15.Level.Portal)
    | DoorTool (Maybe Evergreen.V15.Point.Point)
    | ExitTool


type StartGame
    = NotStarted
        { pressedStart : Bool
        }
    | Started Evergreen.V15.Game.Game


type alias Model =
    { tool : Tool
    , pointerPosition : Maybe ( Float, Float )
    , pointerIsDown : Bool
    , level : Level
    , levelId : Maybe (Evergreen.V15.Id.Id LevelId)
    , undoHistory : List Level
    , redoHistory : List Level
    , game : StartGame
    }


type Msg
    = PressedSelectTool Tool
    | PressedReset
    | PointerMoved Html.Events.Extra.Pointer.Event
    | PointerDown Html.Events.Extra.Pointer.Event
    | PointerUp Html.Events.Extra.Pointer.Event
    | PressedPlay
    | PressedSave
    | GameMsg Evergreen.V15.Game.Msg
    | PressedBackToEditor
    | TypedTimeDelta
        { portalPairIndex : Int
        , timeDelta : Int
        }


type ToBackend
    = SaveLevelRequest Level


type ToFrontend
    = SaveLevelResponse (Evergreen.V15.Id.Id LevelId)
