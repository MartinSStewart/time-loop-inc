module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Dom
import Browser.Navigation exposing (Key)
import Dict as RegularDict
import Keyboard
import Level exposing (Level)
import LevelState exposing (Direction, LevelInstant)
import List.Nonempty exposing (Nonempty)
import Time
import Url exposing (Url)


type FrontendModel
    = Loading Loading_
    | Loaded Loaded_
    | LoadingFailed LoadingFailed_


type alias Loading_ =
    { navigationKey : Key, time : Maybe Time.Posix }


type alias Loaded_ =
    { navigationKey : Key
    , moveActions : List (Maybe Direction)
    , targetTime : Maybe Int
    , viewTime : Float
    , keys : List Keyboard.Key
    , timelineCache : RegularDict.Dict Int LevelInstant
    , futureLevels : List Level
    , currentLevel : Level
    , time : Time.Posix
    }


type alias LoadingFailed_ =
    { error : String }


type alias BackendModel =
    {}


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | KeyMsg Keyboard.Msg
    | PressedNextLevel
    | DraggedTimelineSlider Float
    | SliderLostFocus
    | AnimationFrame Time.Posix


type ToBackend
    = NoOpToBackend


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = NoOpToFrontend
