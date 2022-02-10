module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Dom
import Browser.Navigation exposing (Key)
import Dict as RegularDict
import Editor
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
    , page : Page
    , keys : List Keyboard.Key
    , previousKeys : List Keyboard.Key
    , time : Time.Posix
    }


type Page
    = GamePage Game
    | EditorPage Editor.Model


type alias Game =
    { moveActions : List (Maybe Direction)
    , targetTime : Maybe Int
    , viewTime : Float
    , timelineCache : RegularDict.Dict Int LevelInstant
    , futureLevels : List Level
    , currentLevel : Level
    }


type alias LoadingFailed_ =
    { error : String }


type alias BackendModel =
    {}


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | KeyMsg Keyboard.Msg
    | GameMsg GameMsg
    | EditorMsg Editor.Msg
    | AnimationFrame Time.Posix
    | PressedGotoEditor


type GameMsg
    = PressedNextLevel
    | PressedSkipLevel
    | PressedResetLevel
    | DraggedTimelineSlider Float
    | SliderLostFocus


type ToBackend
    = NoOpToBackend


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = NoOpToFrontend
