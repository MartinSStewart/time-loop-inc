module Route exposing (LevelId, ReplayId, Route(..), parser)

import Id exposing (Id)
import Url.Parser exposing ((</>))


type Route
    = Homepage
    | LevelRoute (Id LevelId)
    | ReplayRoute (Id LevelId) (Id ReplayId)


type LevelId
    = LevelId Never


type ReplayId
    = ReplayId Never


parser : Url.Parser.Parser (Route -> c) c
parser =
    Url.Parser.oneOf
        [ Url.Parser.s "level"
            </> Url.Parser.int
            </> Url.Parser.int
            |> Url.Parser.map (\levelId replayId -> ReplayRoute (Id.fromInt levelId) (Id.fromInt replayId))
        , Url.Parser.s "level" </> Url.Parser.int |> Url.Parser.map (Id.fromInt >> LevelRoute)
        , Url.Parser.top |> Url.Parser.map Homepage
        ]
