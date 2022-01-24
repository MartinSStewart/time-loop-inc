module Evergreen.V1.Types exposing (..)

import Browser
import Browser.Navigation
import Evergreen.V1.Level
import Evergreen.V1.LevelState
import Keyboard
import Url


type alias Loading_ =
    { navigationKey : Browser.Navigation.Key
    }


type alias Loaded_ =
    { navigationKey : Browser.Navigation.Key
    , playerActions : List (Maybe Evergreen.V1.LevelState.MoveAction)
    , currentTime : Maybe Int
    , keys : List Keyboard.Key
    , level : Evergreen.V1.Level.Level
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
