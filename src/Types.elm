module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Keyboard
import Level exposing (Level)
import LevelState exposing (MoveAction)
import List.Nonempty exposing (Nonempty)
import Url exposing (Url)


type FrontendModel
    = Loading Loading_
    | Loaded Loaded_
    | LoadingFailed LoadingFailed_


type alias Loading_ =
    { navigationKey : Key }


type alias Loaded_ =
    { navigationKey : Key
    , moveActions : List (Maybe MoveAction)
    , currentTime : Maybe Int
    , keys : List Keyboard.Key
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
    | PressedTimeMinus
    | PressedTimePlus
    | PressedNextLevel


type ToBackend
    = NoOpToBackend


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = NoOpToFrontend
