module Frontend exposing (..)

import AssocList as Dict exposing (Dict)
import AssocSet as Set exposing (Set)
import Browser
import Browser.Navigation
import Dict as RegularDict
import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import Keyboard exposing (Key)
import Lamdera
import Level exposing (Level, Portal, TileEdge(..))
import LevelState exposing (LevelInstant, MoveAction(..))
import List.Extra as List
import Maybe.Extra as Maybe
import Point exposing (Point)
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
            [ { firstPortal = { position = ( 0, 2 ), tileEdge = LeftEdge }
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
                        Just (Just MoveLeft)

                    else if keyPressed Keyboard.ArrowRight then
                        Just (Just MoveRight)

                    else if keyPressed Keyboard.ArrowUp then
                        Just (Just MoveUp)

                    else if keyPressed Keyboard.ArrowDown then
                        Just (Just MoveDown)

                    else if keyPressed Keyboard.Spacebar then
                        Just Nothing

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


viewLevel : Level -> List (Maybe MoveAction) -> Int -> Element msg
viewLevel level playerActions currentTime =
    let
        ( w, h ) =
            Level.levelSize level

        walls : Set Point
        walls =
            Level.getWalls level

        timeline : RegularDict.Dict Int LevelInstant
        timeline =
            LevelState.timeline level playerActions

        latestInstant =
            RegularDict.keys timeline |> List.maximum |> Maybe.withDefault 0

        earliestInstant =
            RegularDict.keys timeline |> List.minimum |> Maybe.withDefault 0

        current : LevelInstant
        current =
            RegularDict.get (clamp earliestInstant latestInstant currentTime) timeline
                |> Maybe.withDefault { players = [], boxes = [] }

        portals : List { timeDelta : Int, portal : Portal }
        portals =
            Level.portalPairs level
                |> List.concatMap
                    (\portalPair ->
                        [ { timeDelta = portalPair.timeDelta, portal = portalPair.firstPortal }
                        , { timeDelta = -portalPair.timeDelta, portal = portalPair.secondPortal }
                        ]
                    )
    in
    List.range 0 (w - 1)
        |> List.map
            (\x ->
                List.range 0 (h - 1)
                    |> List.map
                        (\y ->
                            let
                                localPortals : List ( Int, TileEdge )
                                localPortals =
                                    List.filterMap
                                        (\portal ->
                                            if portal.portal.position == ( x, y ) then
                                                Just ( portal.timeDelta, portal.portal.tileEdge )

                                            else
                                                Nothing
                                        )
                                        portals

                                borderWidth tileEdge =
                                    if List.any (Tuple.second >> (==) tileEdge) localPortals then
                                        6

                                    else
                                        0
                            in
                            Element.el
                                [ Element.width (Element.px 32)
                                , Element.height (Element.px 32)
                                , Element.Font.center
                                , Element.Border.width 1
                                , Element.el
                                    [ Element.Border.widthEach
                                        { left = borderWidth LeftEdge
                                        , right = borderWidth RightEdge
                                        , top = borderWidth TopEdge
                                        , bottom = borderWidth BottomEdge
                                        }
                                    , Element.Border.color (Element.rgb 0.3 0.3 1)
                                    , Element.width Element.fill
                                    , Element.height Element.fill
                                    ]
                                    Element.none
                                    |> Element.inFront
                                , if Set.member ( x, y ) walls then
                                    Element.Background.color (Element.rgb 0 0 0)

                                  else
                                    Element.Background.color (Element.rgb 1 1 1)
                                ]
                                (case List.find (\player -> player.position == ( x, y )) current.players of
                                    Just player ->
                                        "P" ++ String.fromInt player.age |> Element.text

                                    Nothing ->
                                        if List.any (\box -> box.position == ( x, y )) current.boxes then
                                            Element.text "B"

                                        else
                                            case List.head localPortals of
                                                Just ( timeDelta, _ ) ->
                                                    String.fromInt timeDelta |> Element.text

                                                Nothing ->
                                                    Element.none
                                )
                        )
                    |> Element.column []
            )
        |> Element.row []


viewLoaded : Loaded_ -> Element FrontendMsg
viewLoaded model =
    Element.column
        [ Element.padding 16, Element.spacing 8 ]
        [ viewLevel model.level model.playerActions model.currentTime
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
