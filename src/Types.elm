module Types exposing (..)

import AssocList exposing (Dict)
import Browser
import Editor
import Effect.Browser.Navigation exposing (Key)
import Effect.Time
import Game exposing (Game, Replay)
import Id exposing (Id)
import Keyboard
import Route exposing (LevelId, ReplayId)
import Url exposing (Url)


type FrontendModel
    = Loading Loading_
    | Loaded Loaded_
    | LoadingFailed LoadingFailed_


type alias Loading_ =
    { navigationKey : Effect.Browser.Navigation.Key
    , time : Maybe Effect.Time.Posix
    , levelLoading : Maybe { levelId : Id LevelId, level : Maybe Editor.Level }
    }


type alias Loaded_ =
    { navigationKey : Effect.Browser.Navigation.Key
    , page : Page
    , keys : List Keyboard.Key
    , previousKeys : List Keyboard.Key
    , time : Effect.Time.Posix
    , failedToLoadLevel : Bool
    }


type Page
    = GamePage Game
    | EditorPage Editor.Model


type alias LoadingFailed_ =
    { error : String }


type alias BackendModel =
    { savedLevels : Dict (Id LevelId) { level : Editor.Level, replays : Dict (Id ReplayId) Replay }
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url
    | KeyMsg Keyboard.Msg
    | GameMsg Game.Msg
    | EditorMsg Editor.Msg
    | AnimationFrame Effect.Time.Posix
    | PressedGotoEditor


type ToBackend
    = NoOpToBackend
    | EditorToBackend Editor.ToBackend
    | LoadLevelRequest (Id LevelId)
    | LoadReplayRequest (Id LevelId) (Id ReplayId)


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = NoOpToFrontend
    | EditorToFrontend Editor.ToFrontend
    | LoadLevelResponse (Id LevelId) (Maybe Editor.Level)
    | LoadReplayResponse (Id LevelId) (Id ReplayId) (Maybe { level : Editor.Level, replay : Replay })
