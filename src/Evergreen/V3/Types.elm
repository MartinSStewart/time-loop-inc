module Evergreen.V3.Types exposing (..)

import Browser
import Browser.Navigation
import Evergreen.V3.Level
import Evergreen.V3.LevelState
import Keyboard
import Url


type alias Loading_ =
    { navigationKey : Browser.Navigation.Key
    }


type alias Loaded_ =
    { navigationKey : Browser.Navigation.Key
    , moveActions : List (Maybe Evergreen.V3.LevelState.MoveAction)
    , currentTime : Maybe Int
    , keys : List Keyboard.Key
    , level : Evergreen.V3.Level.Level
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


type ToBackend
    = NoOpToBackend


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = NoOpToFrontend
