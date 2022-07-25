module Frontend exposing (..)

import AssocList as Dict exposing (Dict)
import AssocSet as Set exposing (Set)
import Browser
import Editor
import Effect.Browser.Events
import Effect.Browser.Navigation
import Effect.Command as Command exposing (Command, FrontendOnly)
import Effect.Lamdera
import Effect.Subscription as Subscription exposing (Subscription)
import Element exposing (Element)
import Game
import Id exposing (Id)
import Keyboard exposing (Key)
import Lamdera
import Level exposing (Laser, Level, Portal, TileEdge(..), WallType(..))
import List.Nonempty exposing (Nonempty(..))
import Types exposing (..)
import Url exposing (Url)
import Url.Parser exposing ((</>), (<?>))


app =
    Effect.Lamdera.frontend
        Lamdera.sendToBackend
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = subscriptions
        , view = view
        }


maybeLevels : Maybe (Nonempty Level)
maybeLevels =
    [ levelIntro, level0, level2, laserLevel1, laserLevel2, level1 ]
        |> List.filterMap Result.toMaybe
        |> List.Nonempty.fromList


laserLevel1 =
    Level.init
        { playerStart = ( 0, 2 )
        , walls =
            []
                |> Dict.fromList
        , boxesStart = [ ( 2, 3 ) ] |> Set.fromList
        , exit =
            { position = ( 7, 4 )
            , tileEdge = BottomEdge
            }
        , levelSize = ( 8, 5 )
        , portalPairs =
            []
        , doors = []
        , lasers =
            [ { position = ( 3, 4 ), tileEdge = BottomEdge }
            ]
        }


laserLevel2 =
    Level.init
        { playerStart = ( 0, 2 )
        , walls =
            []
                |> Dict.fromList
        , boxesStart = [ ( 2, 3 ) ] |> Set.fromList
        , exit =
            { position = ( 7, 4 )
            , tileEdge = BottomEdge
            }
        , levelSize = ( 8, 5 )
        , portalPairs =
            [ { firstPortal = { position = ( 5, 4 ), tileEdge = BottomEdge }
              , secondPortal = { position = ( 3, 0 ), tileEdge = TopEdge }
              , timeDelta = -10
              }
            ]
        , doors = []
        , lasers =
            [ { position = ( 3, 4 ), tileEdge = BottomEdge }
            , { position = ( 0, 0 ), tileEdge = LeftEdge }
            ]
        }


laserLevel3 =
    Level.init
        { playerStart = ( 4, 5 )
        , walls =
            [ ( ( 3, 2 ), Wall ) ]
                |> Dict.fromList
        , boxesStart = [ ( 2, 2 ), ( 1, 2 ), ( 4, 4 ) ] |> Set.fromList
        , exit =
            { position = ( 7, 7 )
            , tileEdge = BottomEdge
            }
        , levelSize = ( 8, 8 )
        , portalPairs =
            [ { firstPortal = { position = ( 5, 7 ), tileEdge = BottomEdge }
              , secondPortal = { position = ( 3, 3 ), tileEdge = TopEdge }
              , timeDelta = 20
              }
            ]
        , doors = []
        , lasers =
            [ { position = ( 3, 7 ), tileEdge = BottomEdge }
            ]
        }


levelIntro : Result String Level
levelIntro =
    Level.init
        { playerStart = ( 3, 3 )
        , walls =
            [ ( ( 1, 1 ), Wall )
            , ( ( 2, 1 ), Wall )
            , ( ( 3, 1 ), Wall )
            , ( ( 4, 1 ), Wall )
            , ( ( 0, 1 ), Wall )
            , ( ( 1, 5 ), Wall )
            , ( ( 2, 5 ), Wall )
            , ( ( 3, 5 ), Wall )
            , ( ( 4, 5 ), Wall )
            , ( ( 0, 5 ), Wall )
            , ( ( 0, 2 ), Wall )
            , ( ( 0, 3 ), Wall )
            , ( ( 0, 4 ), Wall )
            , ( ( 4, 2 ), Wall )
            , ( ( 4, 3 ), Wall )
            , ( ( 4, 4 ), Wall )
            ]
                |> Dict.fromList
        , boxesStart = [] |> Set.fromList
        , exit =
            { position = ( 6, 5 )
            , tileEdge = BottomEdge
            }
        , levelSize = ( 8, 6 )
        , portalPairs =
            [ { firstPortal = { position = ( 1, 3 ), tileEdge = LeftEdge }
              , secondPortal = { position = ( 2, 0 ), tileEdge = TopEdge }
              , timeDelta = -6
              }
            ]
        , doors = []
        , lasers = []
        }


level0 : Result String Level
level0 =
    Level.init
        { playerStart = ( 1, 2 )
        , walls = [ ( ( 3, 0 ), Wall ), ( ( 3, 1 ), Wall ), ( ( 3, 3 ), Wall ), ( ( 3, 4 ), Wall ) ] |> Dict.fromList
        , boxesStart = [] |> Set.fromList
        , exit =
            { position = ( 7, 0 )
            , tileEdge = TopEdge
            }
        , levelSize = ( 8, 5 )
        , portalPairs =
            [ { firstPortal = { position = ( 5, 0 ), tileEdge = TopEdge }
              , secondPortal = { position = ( 5, 4 ), tileEdge = BottomEdge }
              , timeDelta = 8
              }
            ]
        , doors = [ { doorPosition = ( 3, 2 ), buttonPosition = ( 6, 2 ) } ]
        , lasers = []
        }


level1 : Result String Level
level1 =
    Level.init
        { playerStart = ( 1, 2 )
        , walls =
            [ ( ( 3, 0 ), Wall )
            , ( ( 3, 1 ), Wall )
            , ( ( 3, 3 ), Wall )
            , ( ( 3, 4 ), Wall )
            , ( ( 4, 0 ), Wall )
            , ( ( 4, 1 ), Wall )
            , ( ( 4, 3 ), Wall )
            , ( ( 4, 4 ), Wall )
            , ( ( 5, 0 ), Wall )
            , ( ( 5, 1 ), Wall )
            , ( ( 5, 3 ), Wall )
            , ( ( 5, 4 ), Wall )
            ]
                |> Dict.fromList
        , boxesStart = [] |> Set.fromList
        , exit =
            { position = ( 7, 2 )
            , tileEdge = RightEdge
            }
        , levelSize = ( 8, 5 )
        , portalPairs =
            [ { firstPortal = { position = ( 0, 1 ), tileEdge = LeftEdge }
              , secondPortal = { position = ( 2, 1 ), tileEdge = RightEdge }
              , timeDelta = 10
              }
            ]
        , doors =
            [ { doorPosition = ( 3, 2 ), buttonPosition = ( 0, 4 ) }
            , { doorPosition = ( 4, 2 ), buttonPosition = ( 1, 4 ) }
            , { doorPosition = ( 5, 2 ), buttonPosition = ( 2, 4 ) }
            ]
        , lasers = []
        }


level2 : Result String Level
level2 =
    Level.init
        { playerStart = ( 1, 2 )
        , walls = [ ( ( 3, 0 ), Wall ), ( ( 3, 1 ), Wall ), ( ( 3, 3 ), Wall ), ( ( 3, 4 ), Wall ) ] |> Dict.fromList
        , boxesStart = [ ( 5, 2 ) ] |> Set.fromList
        , exit =
            { position = ( 7, 0 )
            , tileEdge = TopEdge
            }
        , levelSize = ( 8, 5 )
        , portalPairs =
            [ { firstPortal = { position = ( 5, 0 ), tileEdge = TopEdge }
              , secondPortal = { position = ( 5, 4 ), tileEdge = BottomEdge }
              , timeDelta = 16
              }
            ]
        , doors =
            [ { doorPosition = ( 3, 2 ), buttonPosition = ( 0, 2 ) }
            ]
        , lasers = []
        }


init : Url -> Effect.Browser.Navigation.Key -> ( FrontendModel, Command FrontendOnly ToBackend FrontendMsg )
init url navigationKey =
    let
        urlParser =
            Url.Parser.oneOf
                [ Url.Parser.top |> Url.Parser.map Nothing
                , Url.Parser.s "level" </> Url.Parser.int |> Url.Parser.map (Id.fromInt >> Just)
                ]
    in
    ( Loading { navigationKey = navigationKey, time = Nothing, levelLoading = Nothing }
    , case Url.Parser.parse urlParser url of
        Nothing ->
            Command.none

        Just Nothing ->
            Command.none

        Just (Just levelId) ->
            Effect.Lamdera.sendToBackend (LoadLevelRequest levelId)
    )


initLoaded : Loading_ -> FrontendModel
initLoaded loading =
    case ( maybeLevels, loading.time ) of
        ( Just levels, Just time ) ->
            { navigationKey = loading.navigationKey
            , page = GamePage (Game.init levels)
            , time = time
            , keys = []
            , previousKeys = []
            , failedToLoadLevel = False
            }
                |> (\model ->
                        case loading.levelLoading of
                            Just { levelId, level } ->
                                loadLevel levelId level model

                            Nothing ->
                                model
                   )
                |> Loaded

        ( Just _, _ ) ->
            Loading loading

        ( Nothing, _ ) ->
            LoadingFailed { error = "All levels failed to load" }



---- UPDATE ----


update : FrontendMsg -> FrontendModel -> ( FrontendModel, Command FrontendOnly ToBackend FrontendMsg )
update msg model =
    case model of
        Loading loading ->
            case msg of
                AnimationFrame time ->
                    ( initLoaded { loading | time = Just time }, Command.none )

                _ ->
                    ( model, Command.none )

        Loaded loaded ->
            updateLoaded msg loaded |> Tuple.mapFirst Loaded

        LoadingFailed loadingFailed ->
            ( LoadingFailed loadingFailed, Command.none )


updateLoaded : FrontendMsg -> Loaded_ -> ( Loaded_, Command FrontendOnly ToBackend FrontendMsg )
updateLoaded msg model =
    case msg of
        KeyMsg keyMsg ->
            let
                newKeys =
                    Keyboard.update keyMsg model.keys

                newModel =
                    { model | keys = newKeys, previousKeys = model.keys }
            in
            ( case model.page of
                GamePage game ->
                    { newModel | page = Game.keyUpdate newModel game |> GamePage }

                EditorPage editor ->
                    { newModel | page = Editor.keyUpdate newModel editor |> EditorPage }
            , Command.none
            )

        UrlClicked urlRequest ->
            ( model
            , case urlRequest of
                Browser.Internal url ->
                    Effect.Browser.Navigation.pushUrl model.navigationKey (Url.toString url)

                Browser.External url ->
                    Effect.Browser.Navigation.load url
            )

        UrlChanged url ->
            ( model, Command.none )

        GameMsg gameMsg ->
            ( case model.page of
                GamePage game ->
                    { model | page = Game.update gameMsg game |> GamePage }

                _ ->
                    model
            , Command.none
            )

        EditorMsg editorMsg ->
            case model.page of
                EditorPage editor ->
                    Editor.update editorMsg editor
                        |> Tuple.mapBoth
                            (\newEditor -> { model | page = EditorPage newEditor })
                            (\cmd -> Command.map EditorToBackend EditorMsg cmd)

                _ ->
                    ( model, Command.none )

        AnimationFrame _ ->
            ( case model.page of
                GamePage game ->
                    { model | page = Game.animationFrame game |> GamePage }

                EditorPage editor ->
                    { model | page = Editor.animationFrame editor |> EditorPage }
            , Command.none
            )

        PressedGotoEditor ->
            ( { model | page = Editor.init |> EditorPage }
            , Command.none
            )


updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Command FrontendOnly ToBackend FrontendMsg )
updateFromBackend msg model =
    case model of
        Loaded loaded ->
            updateFromBackendLoaded msg loaded
                |> Tuple.mapFirst Loaded

        Loading loading ->
            case msg of
                LoadLevelResponse levelId maybeLevel ->
                    ( { loading | levelLoading = Just { levelId = levelId, level = maybeLevel } } |> Loading
                    , Command.none
                    )

                _ ->
                    ( model, Command.none )

        LoadingFailed _ ->
            ( model, Command.none )


updateFromBackendLoaded : ToFrontend -> Loaded_ -> ( Loaded_, Command FrontendOnly ToBackend FrontendMsg )
updateFromBackendLoaded msg model =
    case msg of
        NoOpToFrontend ->
            ( model, Command.none )

        EditorToFrontend toFrontend ->
            case model.page of
                EditorPage editor ->
                    Editor.updateFromBackend model.navigationKey toFrontend editor
                        |> Tuple.mapBoth
                            (\newEditor -> { model | page = EditorPage newEditor })
                            (\cmd -> Command.map EditorToBackend EditorMsg cmd)

                GamePage game ->
                    ( model, Command.none )

        LoadLevelResponse levelId maybeLevel ->
            ( loadLevel levelId maybeLevel model, Command.none )


loadLevel : Id Editor.LevelId -> Maybe Editor.Level -> Loaded_ -> Loaded_
loadLevel levelId maybeLevel model =
    case maybeLevel of
        Just level ->
            { model | page = Editor.initWithLevel levelId level |> EditorPage }

        Nothing ->
            { model | failedToLoadLevel = True }



---- VIEW ----


view : FrontendModel -> Browser.Document FrontendMsg
view model =
    { title = "Time travel game"
    , body =
        [ Element.layout
            []
            (case model of
                Loading _ ->
                    Element.none

                Loaded loaded ->
                    case loaded.page of
                        GamePage game ->
                            Element.column
                                []
                                [ Game.view game |> Element.map GameMsg
                                , Game.button
                                    Game.buttonAttributes
                                    { onPress = PressedGotoEditor
                                    , label = Element.text "Go to level editor"
                                    }
                                ]

                        EditorPage editor ->
                            Editor.view editor |> Element.map EditorMsg

                LoadingFailed { error } ->
                    Element.text error
            )
        ]
    }


subscriptions : FrontendModel -> Subscription FrontendOnly FrontendMsg
subscriptions model =
    Subscription.batch
        [ Subscription.map KeyMsg Keyboard.subscriptions
        , Effect.Browser.Events.onAnimationFrame AnimationFrame
        ]
