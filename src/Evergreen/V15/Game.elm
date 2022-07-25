module Evergreen.V15.Game exposing (..)

import Dict
import Evergreen.V15.Level
import Evergreen.V15.LevelState


type alias Game =
    { moveActions : List (Maybe Evergreen.V15.LevelState.Direction)
    , targetTime : Maybe Int
    , viewTime : Float
    , timelineCache : Dict.Dict Int Evergreen.V15.LevelState.LevelInstant
    , futureLevels : List Evergreen.V15.Level.Level
    , currentLevel : Evergreen.V15.Level.Level
    }


type Msg
    = PressedNextLevel
    | PressedSkipLevel
    | PressedResetLevel
    | DraggedTimelineSlider Float
    | SliderLostFocus
