module Evergreen.V16.Types exposing (..)

import AssocList
import Browser
import Effect.Browser.Navigation
import Effect.Time
import Evergreen.V16.Editor
import Evergreen.V16.Game
import Evergreen.V16.Id
import Evergreen.V16.Keyboard
import Evergreen.V16.Route
import Url


type alias Loading_ =
    { navigationKey : Effect.Browser.Navigation.Key
    , time : Maybe Effect.Time.Posix
    , levelLoading :
        Maybe
            { levelId : Evergreen.V16.Id.Id Evergreen.V16.Route.LevelId
            , level : Maybe Evergreen.V16.Editor.Level
            }
    }


type Page
    = GamePage Evergreen.V16.Game.Game
    | EditorPage Evergreen.V16.Editor.Model


type alias Loaded_ =
    { navigationKey : Effect.Browser.Navigation.Key
    , page : Page
    , keys : List Evergreen.V16.Keyboard.Key
    , previousKeys : List Evergreen.V16.Keyboard.Key
    , time : Effect.Time.Posix
    , failedToLoadLevel : Bool
    }


type alias LoadingFailed_ =
    { error : String
    }


type FrontendModel
    = Loading Loading_
    | Loaded Loaded_
    | LoadingFailed LoadingFailed_


type alias BackendModel =
    { savedLevels :
        AssocList.Dict
            (Evergreen.V16.Id.Id Evergreen.V16.Route.LevelId)
            { level : Evergreen.V16.Editor.Level
            , replays : AssocList.Dict (Evergreen.V16.Id.Id Evergreen.V16.Route.ReplayId) Evergreen.V16.Game.Replay
            }
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | KeyMsg Evergreen.V16.Keyboard.Msg
    | GameMsg Evergreen.V16.Game.Msg
    | EditorMsg Evergreen.V16.Editor.Msg
    | AnimationFrame Effect.Time.Posix
    | PressedGotoEditor


type ToBackend
    = NoOpToBackend
    | EditorToBackend Evergreen.V16.Editor.ToBackend
    | LoadLevelRequest (Evergreen.V16.Id.Id Evergreen.V16.Route.LevelId)
    | LoadReplayRequest (Evergreen.V16.Id.Id Evergreen.V16.Route.LevelId) (Evergreen.V16.Id.Id Evergreen.V16.Route.ReplayId)


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = NoOpToFrontend
    | EditorToFrontend Evergreen.V16.Editor.ToFrontend
    | LoadLevelResponse (Evergreen.V16.Id.Id Evergreen.V16.Route.LevelId) (Maybe Evergreen.V16.Editor.Level)
    | LoadReplayResponse
        (Evergreen.V16.Id.Id Evergreen.V16.Route.LevelId)
        (Evergreen.V16.Id.Id Evergreen.V16.Route.ReplayId)
        (Maybe
            { level : Evergreen.V16.Editor.Level
            , replay : Evergreen.V16.Game.Replay
            }
        )
