module Evergreen.V9.Types exposing (..)

import Browser
import Browser.Navigation
import Evergreen.V9.Level
import Evergreen.V9.LevelState
import Keyboard
import Url


type alias Loading_ =
    { navigationKey : Browser.Navigation.Key
    }


type alias Loaded_ =
    { navigationKey : Browser.Navigation.Key
    , moveActions : List (Maybe Evergreen.V9.LevelState.MoveAction)
    , currentTime : Maybe Int
    , keys : List Keyboard.Key
    , futureLevels : List Evergreen.V9.Level.Level
    , currentLevel : Evergreen.V9.Level.Level
    }


type alias LoadingFailed_ =
    { error : String
    }


type FrontendModel
    = Loading Loading_
    | Loaded Loaded_
    | LoadingFailed LoadingFailed_


type alias BackendModel =
    {}


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | KeyMsg Keyboard.Msg
    | PressedTimeMinus
    | PressedTimePlus
    | PressedNextLevel


type ToBackend
    = NoOpToBackend


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = NoOpToFrontend
