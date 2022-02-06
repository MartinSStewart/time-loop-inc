module Evergreen.V12.Types exposing (..)

import Browser
import Browser.Navigation
import Dict
import Evergreen.V12.Level
import Evergreen.V12.LevelState
import Keyboard
import Time
import Url


type alias Loading_ =
    { navigationKey : Browser.Navigation.Key
    , time : Maybe Time.Posix
    }


type alias Loaded_ =
    { navigationKey : Browser.Navigation.Key
    , moveActions : List (Maybe Evergreen.V12.LevelState.Direction)
    , targetTime : Maybe Int
    , viewTime : Float
    , keys : List Keyboard.Key
    , timelineCache : Dict.Dict Int Evergreen.V12.LevelState.LevelInstant
    , futureLevels : List Evergreen.V12.Level.Level
    , currentLevel : Evergreen.V12.Level.Level
    , time : Time.Posix
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
    | PressedNextLevel
    | PressedSkipLevel
    | PressedResetLevel
    | DraggedTimelineSlider Float
    | SliderLostFocus
    | AnimationFrame Time.Posix


type ToBackend
    = NoOpToBackend


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = NoOpToFrontend
