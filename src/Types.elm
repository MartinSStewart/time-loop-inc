module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Keyboard
import Level exposing (Level)
import LevelState exposing (MoveAction)
import Url exposing (Url)


type FrontendModel
    = Loading Loading_
    | Loaded Loaded_
    | LoadingFailed LoadingFailed_


type alias Loading_ =
    { navigationKey : Key }


type alias Loaded_ =
    { navigationKey : Key
    , playerActions : List MoveAction
    , currentTime : Int
    , keys : List Keyboard.Key
    , level : Level
    }


type alias LoadingFailed_ =
    { error : String }


type alias BackendModel =
    { message : String
    }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | KeyMsg Keyboard.Msg


type ToBackend
    = NoOpToBackend


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = NoOpToFrontend
