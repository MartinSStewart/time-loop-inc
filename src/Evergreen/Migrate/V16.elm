module Evergreen.Migrate.V16 exposing (..)

import AssocList as Dict exposing (Dict)
import AssocSet as Set
import Evergreen.V15.Editor
import Evergreen.V15.Id
import Evergreen.V15.Level
import Evergreen.V15.Types as Old
import Evergreen.V16.Editor
import Evergreen.V16.Id
import Evergreen.V16.Level
import Evergreen.V16.Types as New
import Lamdera.Migrations exposing (..)


migrateDoor : Evergreen.V15.Level.Door -> Evergreen.V16.Level.Door
migrateDoor old =
    { doorPosition = old.doorPosition, buttonPosition = old.buttonPosition }


migrateExit : Evergreen.V15.Level.Exit -> Evergreen.V16.Level.Exit
migrateExit old =
    { position = old.position, tileEdge = migrateTileEdge old.tileEdge }


migrateLaser : Evergreen.V15.Level.Laser -> Evergreen.V16.Level.Laser
migrateLaser old =
    { position = old.position, tileEdge = migrateTileEdge old.tileEdge }


migratePortal : Evergreen.V15.Level.Portal -> Evergreen.V16.Level.Portal
migratePortal old =
    { position = old.position, tileEdge = migrateTileEdge old.tileEdge }


migratePortalPair : Evergreen.V15.Level.PortalPair -> Evergreen.V16.Level.PortalPair
migratePortalPair old =
    { firstPortal = migratePortal old.firstPortal
    , secondPortal = migratePortal old.secondPortal
    , timeDelta = old.timeDelta
    }


migrateTileEdge : Evergreen.V15.Level.TileEdge -> Evergreen.V16.Level.TileEdge
migrateTileEdge old =
    case old of
        Evergreen.V15.Level.TopEdge ->
            Evergreen.V16.Level.TopEdge

        Evergreen.V15.Level.BottomEdge ->
            Evergreen.V16.Level.BottomEdge

        Evergreen.V15.Level.LeftEdge ->
            Evergreen.V16.Level.LeftEdge

        Evergreen.V15.Level.RightEdge ->
            Evergreen.V16.Level.RightEdge


migrateWallType : Evergreen.V15.Level.WallType -> Evergreen.V16.Level.WallType
migrateWallType old =
    case old of
        Evergreen.V15.Level.Wall ->
            Evergreen.V16.Level.Wall

        Evergreen.V15.Level.Glass ->
            Evergreen.V16.Level.Glass


migrateEditorLevel : Evergreen.V15.Editor.Level -> Evergreen.V16.Editor.Level
migrateEditorLevel old =
    { playerStart = old.playerStart
    , walls = migrateDict identity migrateWallType old.walls
    , boxesStart = old.boxesStart
    , exit = Maybe.map migrateExit old.exit
    , levelSize = old.levelSize
    , portalPairs = List.map migratePortalPair old.portalPairs
    , doors = List.map migrateDoor old.doors
    , lasers = List.map migrateLaser old.lasers
    }


frontendModel : Old.FrontendModel -> ModelMigration New.FrontendModel New.FrontendMsg
frontendModel old =
    ModelUnchanged


backendModel : Old.BackendModel -> ModelMigration New.BackendModel New.BackendMsg
backendModel old =
    ModelMigrated
        ( { savedLevels =
                migrateDict
                    migrateId
                    (\level -> { level = migrateEditorLevel level, replays = Dict.empty })
                    old.savedLevels
          }
        , Cmd.none
        )


migrateId : Evergreen.V15.Id.Id a -> Evergreen.V16.Id.Id b
migrateId (Evergreen.V15.Id.Id id) =
    Evergreen.V16.Id.Id id


migrateDict : (a -> c) -> (b -> d) -> Dict a b -> Dict c d
migrateDict migrateKey migrateValue dict =
    Dict.toList dict |> List.map (Tuple.mapBoth migrateKey migrateValue) |> Dict.fromList


frontendMsg : Old.FrontendMsg -> MsgMigration New.FrontendMsg New.FrontendMsg
frontendMsg old =
    MsgOldValueIgnored


toBackend : Old.ToBackend -> MsgMigration New.ToBackend New.BackendMsg
toBackend old =
    MsgOldValueIgnored


backendMsg : Old.BackendMsg -> MsgMigration New.BackendMsg New.BackendMsg
backendMsg old =
    MsgUnchanged


toFrontend : Old.ToFrontend -> MsgMigration New.ToFrontend New.FrontendMsg
toFrontend old =
    MsgOldValueIgnored
