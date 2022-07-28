module Evergreen.V16.Game exposing (..)

import Dict
import Evergreen.V16.Level
import Evergreen.V16.LevelState


type alias Game =
    { moveActions : List (Maybe Evergreen.V16.LevelState.Direction)
    , targetTime : Maybe Int
    , viewTime : Float
    , timelineCache : Dict.Dict Int Evergreen.V16.LevelState.LevelInstant
    , futureLevels : List Evergreen.V16.Level.Level
    , currentLevel : Evergreen.V16.Level.Level
    }


type alias Replay =
    { moveActions : List (Maybe Evergreen.V16.LevelState.Direction)
    }


type Msg
    = PressedNextLevel
    | PressedSkipLevel
    | PressedResetLevel
    | DraggedTimelineSlider Float
    | SliderLostFocus
