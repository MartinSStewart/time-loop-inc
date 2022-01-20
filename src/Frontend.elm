module Frontend exposing (..)

import Browser
import Browser.Navigation
import Element exposing (Element)
import Keyboard exposing (Key)
import Lamdera
import Level exposing (Level, TileEdge(..))
import LevelState exposing (MoveAction(..))
import Maybe.Extra as Maybe
import Set exposing (Set)
import Types exposing (..)
import Url exposing (Url)


app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = subscriptions
        , view = view
        }



---- MODEL ----


getLevel : Result String Level
getLevel =
    Level.init
        { playerStart = ( 1, 1 )
        , walls = [ ( 0, 0 ), ( 0, 1 ) ] |> Set.fromList
        , boxesStart = [ ( 2, 1 ) ] |> Set.fromList
        , exit =
            { position = ( 3, 1 )
            , tileEdge = LeftEdge
            }
        , levelSize = ( 5, 5 )
        , portalPairs =
            [ { firstPortal = { position = ( 2, 3 ), tileEdge = LeftEdge }
              , secondPortal = { position = ( 4, 3 ), tileEdge = RightEdge }
              , timeDelta = 2
              }
            ]
        }


init : Url -> Browser.Navigation.Key -> ( FrontendModel, Cmd FrontendMsg )
init url navigationKey =
    ( { navigationKey = navigationKey } |> initLoaded
    , Cmd.none
    )


initLoaded : Loading_ -> FrontendModel
initLoaded loading =
    case getLevel of
        Ok level ->
            { navigationKey = loading.navigationKey
            , playerActions = []
            , currentTime = 0
            , keys = []
            , level = level
            }
                |> Loaded

        Err error ->
            LoadingFailed { error = error }



---- UPDATE ----


update : FrontendMsg -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
update msg model =
    case model of
        Loading loading ->
            ( Loading loading, Cmd.none )

        Loaded loaded ->
            updateLoaded msg loaded |> Tuple.mapFirst Loaded

        LoadingFailed loadingFailed ->
            ( LoadingFailed loadingFailed, Cmd.none )


updateLoaded : FrontendMsg -> Loaded_ -> ( Loaded_, Cmd FrontendMsg )
updateLoaded msg model =
    case msg of
        KeyMsg keyMsg ->
            let
                newKeys =
                    Keyboard.update keyMsg model.keys

                keyDown key =
                    List.any ((==) key) newKeys

                keyPressed : Keyboard.Key -> Bool
                keyPressed key =
                    keyDown key && (List.any ((==) key) model.keys |> not)

                model_ =
                    case ( keyDown Keyboard.Control, keyPressed (Keyboard.Character "Z") ) of
                        ( True, True ) ->
                            { model
                                | playerActions =
                                    model.playerActions |> List.reverse |> List.drop 1 |> List.reverse
                            }

                        _ ->
                            model

                action =
                    if keyPressed Keyboard.ArrowLeft then
                        Just MoveLeft

                    else if keyPressed Keyboard.ArrowRight then
                        Just MoveRight

                    else if keyPressed Keyboard.ArrowUp then
                        Just MoveUp

                    else if keyPressed Keyboard.ArrowDown then
                        Just MoveDown

                    else if keyPressed Keyboard.Spacebar then
                        Just MoveNone

                    else
                        Nothing
            in
            { model_ | keys = newKeys, playerActions = model_.playerActions ++ Maybe.toList action }
                |> addCmdNone

        UrlClicked urlRequest ->
            Debug.todo ""

        UrlChanged url ->
            Debug.todo ""


updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        NoOpToFrontend ->
            ( model, Cmd.none )


addCmdNone : a -> ( a, Cmd msg )
addCmdNone a =
    ( a, Cmd.none )


addCmd : Cmd msg -> a -> ( a, Cmd msg )
addCmd cmd a =
    ( a, cmd )



---- VIEW ----


tempTimeState =
    { playerPrime = ( ( 1, 1 ), [] )
    , players = []
    , boxes = []
    }


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
                    viewLoaded loaded

                LoadingFailed { error } ->
                    Element.text error
            )
        ]
    }


viewLoaded : Loaded_ -> Element FrontendMsg
viewLoaded model =
    Element.none


subscriptions : FrontendModel -> Sub FrontendMsg
subscriptions model =
    Sub.batch
        [ Sub.map KeyMsg Keyboard.subscriptions
        ]
