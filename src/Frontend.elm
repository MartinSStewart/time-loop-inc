module Frontend exposing (..)

import Browser
import Browser.Navigation
import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import Keyboard exposing (Key)
import Lamdera
import Level exposing (Level, TileEdge(..))
import LevelState exposing (LevelInstant, MoveAction(..))
import List.Nonempty exposing (Nonempty)
import Maybe.Extra as Maybe
import Point exposing (Point)
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
                                , currentTime = model.currentTime - 1
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
            ( { model_
                | keys = newKeys
                , playerActions = model_.playerActions ++ Maybe.toList action
                , currentTime =
                    case action of
                        Just _ ->
                            model_.currentTime + 1

                        Nothing ->
                            model_.currentTime
              }
            , Cmd.none
            )

        UrlClicked urlRequest ->
            Debug.todo ""

        UrlChanged url ->
            Debug.todo ""

        PressedTimeMinus ->
            ( { model | currentTime = model.currentTime - 1 }, Cmd.none )

        PressedTimePlus ->
            ( { model | currentTime = model.currentTime + 1 }, Cmd.none )


updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        NoOpToFrontend ->
            ( model, Cmd.none )



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
                    viewLoaded loaded

                LoadingFailed { error } ->
                    Element.text error
            )
        ]
    }


viewLoaded : Loaded_ -> Element FrontendMsg
viewLoaded model =
    let
        ( w, h ) =
            Level.levelSize model.level

        walls : Set Point
        walls =
            Level.getWalls model.level

        timeline : Nonempty LevelInstant
        timeline =
            LevelState.instant model.level model.playerActions

        current : LevelInstant
        current =
            List.Nonempty.get (clamp 0 (List.Nonempty.length timeline - 1) model.currentTime) timeline
    in
    Element.column
        [ Element.padding 16, Element.spacing 8 ]
        [ List.range 0 (w - 1)
            |> List.map
                (\x ->
                    List.range 0 (h - 1)
                        |> List.map
                            (\y ->
                                Element.el
                                    [ Element.width (Element.px 32)
                                    , Element.height (Element.px 32)
                                    , Element.Font.center
                                    , Element.Border.width 1
                                    , if Set.member ( x, y ) walls then
                                        Element.Background.color (Element.rgb 0 0 0)

                                      else
                                        Element.Background.color (Element.rgb 1 1 1)
                                    ]
                                    (if List.any (\player -> player.position == ( x, y )) current.players then
                                        Element.text "P"

                                     else if List.any (\box -> box.position == ( x, y )) current.boxes then
                                        Element.text "B"

                                     else
                                        Element.none
                                    )
                            )
                        |> Element.column []
                )
            |> Element.row []
        , Element.row
            [ Element.spacing 8 ]
            [ button
                [ Element.padding 8
                , Element.Background.color (Element.rgb 0.7 0.7 0.7)
                ]
                { onPress = PressedTimeMinus, label = Element.text "-" }
            , Element.text ("Time: " ++ String.fromInt model.currentTime)
            , button
                [ Element.padding 8
                , Element.Background.color (Element.rgb 0.7 0.7 0.7)
                ]
                { onPress = PressedTimePlus, label = Element.text "+" }
            ]
        ]


button attributes { onPress, label } =
    Element.Input.button
        attributes
        { onPress = Just onPress
        , label = label
        }


subscriptions : FrontendModel -> Sub FrontendMsg
subscriptions model =
    Sub.batch
        [ Sub.map KeyMsg Keyboard.subscriptions
        ]
