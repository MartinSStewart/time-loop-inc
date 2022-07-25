module Backend exposing (..)

import AssocList as Dict
import Editor exposing (LevelId)
import Effect.Command as Command exposing (BackendOnly, Command)
import Effect.Lamdera exposing (ClientId, SessionId)
import Effect.Subscription as Subscription exposing (Subscription)
import Id exposing (Id)
import Lamdera
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
                newId : Id LevelId
                newId =
                    case Dict.keys model.savedLevels |> List.map Id.toInt |> List.maximum of
                        Just maximum ->
                            maximum + 1 |> Id.fromInt

                        Nothing ->
                            Id.fromInt 1000
            in
            ( { model | savedLevels = Dict.insert newId level model.savedLevels }
            , Effect.Lamdera.sendToFrontend clientId (EditorToFrontend (Editor.SaveLevelResponse newId |> Debug.log "saved"))
            )

        LoadLevelRequest id ->
            ( model
            , Effect.Lamdera.sendToFrontend clientId (LoadLevelResponse id (Dict.get id model.savedLevels))
            )
