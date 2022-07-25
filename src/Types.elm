module Types exposing (..)

import AssocList exposing (Dict)
import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Editor
import Game exposing (Game)
import Keyboard
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


type alias LoadingFailed_ =
    { error : String }


type alias BackendModel =
    { savedLevels : Dict Int Editor.Level
    }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | KeyMsg Keyboard.Msg
    | GameMsg Game.Msg
    | EditorMsg Editor.Msg
    | AnimationFrame Time.Posix
    | PressedGotoEditor


type ToBackend
    = NoOpToBackend


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = NoOpToFrontend
