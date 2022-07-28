module Evergreen.V16.Editor exposing (..)

import AssocList
import AssocSet
import Evergreen.V16.Game
import Evergreen.V16.Id
import Evergreen.V16.Level
import Evergreen.V16.Point
import Evergreen.V16.Route
import Html.Events.Extra.Pointer


type alias Level =
    { playerStart : Maybe Evergreen.V16.Point.Point
    , walls : AssocList.Dict Evergreen.V16.Point.Point Evergreen.V16.Level.WallType
    , boxesStart : AssocSet.Set Evergreen.V16.Point.Point
    , exit : Maybe Evergreen.V16.Level.Exit
    , levelSize : Evergreen.V16.Point.Point
    , portalPairs : List Evergreen.V16.Level.PortalPair
    , doors : List Evergreen.V16.Level.Door
    , lasers : List Evergreen.V16.Level.Laser
    }


type Tool
    = WallTool
    | GlassTool
    | PlayerTool
    | BoxTool
    | EraseTool
    | LaserTool
    | PortalTool (Maybe Evergreen.V16.Level.Portal)
    | DoorTool (Maybe Evergreen.V16.Point.Point)
    | ExitTool


type SaveStatus
    = NotSaved
    | LevelSaved (Evergreen.V16.Id.Id Evergreen.V16.Route.LevelId)
    | ReplaySaved (Evergreen.V16.Id.Id Evergreen.V16.Route.LevelId) (Evergreen.V16.Id.Id Evergreen.V16.Route.ReplayId)


type alias LevelAndId =
    { saveStatus : SaveStatus
    , level : Level
    }


type StartGame
    = NotStarted
        { pressedStart : Bool
        }
    | Started
        { game : Evergreen.V16.Game.Game
        , replay : Maybe Evergreen.V16.Game.Replay
        }


type alias Model =
    { tool : Tool
    , pointerPosition : Maybe ( Float, Float )
    , pointerIsDown : Bool
    , level : LevelAndId
    , undoHistory : List LevelAndId
    , redoHistory : List LevelAndId
    , game : StartGame
    , isSaving : Bool
    }


type Msg
    = PressedSelectTool Tool
    | PressedReset
    | PointerMoved Html.Events.Extra.Pointer.Event
    | PointerDown Html.Events.Extra.Pointer.Event
    | PointerUp Html.Events.Extra.Pointer.Event
    | PressedPlay
    | PressedSave
    | GameMsg Evergreen.V16.Game.Msg
    | PressedBackToEditor
    | PressedSaveRelayAndGoBackToEditor
    | TypedTimeDelta
        { portalPairIndex : Int
        , timeDelta : Int
        }


type ToBackend
    = SaveLevelRequest Level
    | SaveReplayRequest (Evergreen.V16.Id.Id Evergreen.V16.Route.LevelId) Evergreen.V16.Game.Replay
    | SaveLevelAndReplayRequest Level Evergreen.V16.Game.Replay


type ToFrontend
    = SaveLevelResponse (Evergreen.V16.Id.Id Evergreen.V16.Route.LevelId)
    | SaveReplayResponse (Evergreen.V16.Id.Id Evergreen.V16.Route.LevelId) (Result () (Evergreen.V16.Id.Id Evergreen.V16.Route.ReplayId))
    | SaveLevelAndReplayResponse (Evergreen.V16.Id.Id Evergreen.V16.Route.LevelId) (Evergreen.V16.Id.Id Evergreen.V16.Route.ReplayId)
