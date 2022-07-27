module Backend exposing (..)

import AssocList as Dict exposing (Dict)
import Editor
import Effect.Command as Command exposing (BackendOnly, Command)
import Effect.Lamdera exposing (ClientId, SessionId)
import Effect.Subscription as Subscription exposing (Subscription)
import Id exposing (Id)
import Lamdera
import Route exposing (LevelId, ReplayId)
import Types exposing (..)


app =
    Effect.Lamdera.backend
        Lamdera.broadcast
        Lamdera.sendToFrontend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = \m -> Subscription.none
        }


init : ( BackendModel, Command restriction toMsg BackendMsg )
init =
    ( { savedLevels = Dict.empty }
    , Command.none
    )


update : BackendMsg -> BackendModel -> ( BackendModel, Command restriction toMsg BackendMsg )
update msg model =
    case msg of
        NoOpBackendMsg ->
            ( model, Command.none )


updateFromFrontend :
    SessionId
    -> ClientId
    -> ToBackend
    -> BackendModel
    -> ( BackendModel, Command BackendOnly ToFrontend BackendMsg )
updateFromFrontend sessionId clientId msg model =
    case msg of
        NoOpToBackend ->
            ( model, Command.none )

        EditorToBackend (Editor.SaveLevelRequest level) ->
            let
                levelId : Id LevelId
                levelId =
                    newId 1000 model.savedLevels
            in
            ( { model | savedLevels = Dict.insert levelId { level = level, replays = Dict.empty } model.savedLevels }
            , Effect.Lamdera.sendToFrontend clientId (EditorToFrontend (Editor.SaveLevelResponse levelId))
            )

        EditorToBackend (Editor.SaveReplayRequest levelId replay) ->
            case Dict.get levelId model.savedLevels of
                Just level ->
                    let
                        replayId =
                            newId 0 level.replays
                    in
                    ( { model
                        | savedLevels =
                            Dict.insert
                                levelId
                                { level | replays = Dict.insert replayId replay level.replays }
                                model.savedLevels
                      }
                    , EditorToFrontend (Editor.SaveReplayResponse levelId (Ok replayId))
                        |> Effect.Lamdera.sendToFrontend clientId
                    )

                Nothing ->
                    ( model
                    , EditorToFrontend (Editor.SaveReplayResponse levelId (Err ()))
                        |> Effect.Lamdera.sendToFrontend clientId
                    )

        EditorToBackend (Editor.SaveLevelAndReplayRequest level replay) ->
            let
                levelId : Id LevelId
                levelId =
                    newId 1000 model.savedLevels

                replayId : Id ReplayId
                replayId =
                    Id.fromInt 0
            in
            ( { model
                | savedLevels =
                    Dict.insert
                        levelId
                        { level = level, replays = Dict.singleton replayId replay }
                        model.savedLevels
              }
            , EditorToFrontend (Editor.SaveLevelAndReplayResponse levelId replayId)
                |> Effect.Lamdera.sendToFrontend clientId
            )

        LoadLevelRequest id ->
            ( model
            , Effect.Lamdera.sendToFrontend
                clientId
                (LoadLevelResponse id (Dict.get id model.savedLevels |> Maybe.map .level))
            )

        LoadReplayRequest levelId replayId ->
            ( model
            , LoadReplayResponse
                levelId
                replayId
                (case Dict.get levelId model.savedLevels of
                    Just savedLevel ->
                        case Dict.get replayId savedLevel.replays of
                            Just replay ->
                                Just { level = savedLevel.level, replay = replay }

                            Nothing ->
                                Nothing

                    Nothing ->
                        Nothing
                )
                |> Effect.Lamdera.sendToFrontend clientId
            )


newId : Int -> Dict (Id a) b -> Id a
newId startId dict =
    case Dict.keys dict |> List.map Id.toInt |> List.maximum of
        Just maximum ->
            maximum + 1 |> Id.fromInt

        Nothing ->
            Id.fromInt startId
