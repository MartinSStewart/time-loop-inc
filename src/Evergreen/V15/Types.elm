module Evergreen.V15.Types exposing (..)

import AssocList
import Browser
import Effect.Browser.Navigation
import Effect.Time
import Evergreen.V15.Editor
import Evergreen.V15.Game
import Evergreen.V15.Id
import Evergreen.V15.Keyboard
import Url


type alias Loading_ =
    { navigationKey : Effect.Browser.Navigation.Key
    , time : Maybe Effect.Time.Posix
    , levelLoading :
        Maybe
            { levelId : Evergreen.V15.Id.Id Evergreen.V15.Editor.LevelId
            , level : Maybe Evergreen.V15.Editor.Level
            }
    }


type Page
    = GamePage Evergreen.V15.Game.Game
    | EditorPage Evergreen.V15.Editor.Model


type alias Loaded_ =
    { navigationKey : Effect.Browser.Navigation.Key
    , page : Page
    , keys : List Evergreen.V15.Keyboard.Key
    , previousKeys : List Evergreen.V15.Keyboard.Key
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
    { savedLevels : AssocList.Dict (Evergreen.V15.Id.Id Evergreen.V15.Editor.LevelId) Evergreen.V15.Editor.Level
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | KeyMsg Evergreen.V15.Keyboard.Msg
    | GameMsg Evergreen.V15.Game.Msg
    | EditorMsg Evergreen.V15.Editor.Msg
    | AnimationFrame Effect.Time.Posix
    | PressedGotoEditor


type ToBackend
    = NoOpToBackend
    | EditorToBackend Evergreen.V15.Editor.ToBackend
    | LoadLevelRequest (Evergreen.V15.Id.Id Evergreen.V15.Editor.LevelId)


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = NoOpToFrontend
    | EditorToFrontend Evergreen.V15.Editor.ToFrontend
    | LoadLevelResponse (Evergreen.V15.Id.Id Evergreen.V15.Editor.LevelId) (Maybe Evergreen.V15.Editor.Level)
