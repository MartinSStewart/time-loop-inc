module Frontend exposing (..)

import AssocList as Dict exposing (Dict)
import AssocSet as Set exposing (Set)
import Browser
import Browser.Events
import Browser.Navigation
import Dict as RegularDict
import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import Element.Keyed
import Keyboard exposing (Key)
import Lamdera
import Level exposing (Laser, Level, Portal, TileEdge(..), WallType(..))
import LevelState exposing (Direction(..), DoorInstant, LaserBeam, LevelInstant, Paradox, PlayerInstant)
import List.Extra as List
import List.Nonempty exposing (Nonempty(..))
import Maybe.Extra as Maybe
import Point exposing (Point)
import StringExtra as String
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


maybeLevels : Maybe (Nonempty Level)
maybeLevels =
    [ levelIntro, level0, level2, laserLevel1, laserLevel2, level1 ]
        --[ level2 ]
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


init : Url -> Browser.Navigation.Key -> ( FrontendModel, Cmd FrontendMsg )
init _ navigationKey =
    ( Loading { navigationKey = navigationKey, time = Nothing }
    , Cmd.none
    )


initLoaded : Loading_ -> FrontendModel
initLoaded loading =
    case ( maybeLevels, loading.time ) of
        ( Just levels, Just time ) ->
            { navigationKey = loading.navigationKey
            , moveActions = []
            , targetTime = Nothing
            , viewTime = 0
            , keys = []
            , timelineCache = LevelState.timeline (List.Nonempty.head levels) []
            , futureLevels = List.Nonempty.tail levels
            , currentLevel = List.Nonempty.head levels
            , time = time
            }
                |> Loaded

        ( Just _, _ ) ->
            Loading loading

        ( Nothing, _ ) ->
            LoadingFailed { error = "All levels failed to load" }


setMoveActions : List (Maybe Direction) -> Loaded_ -> Loaded_
setMoveActions moveActions loaded_ =
    { loaded_ | moveActions = moveActions, timelineCache = LevelState.timeline loaded_.currentLevel moveActions }



---- UPDATE ----


update : FrontendMsg -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
update msg model =
    case model of
        Loading loading ->
            case msg of
                AnimationFrame time ->
                    ( initLoaded { loading | time = Just time }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

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

                maybeMoveAction =
                    if LevelState.isCompleted model.currentLevel model.timelineCache model.moveActions then
                        Nothing

                    else if keyPressed Keyboard.ArrowLeft || keyPressed (Keyboard.Character "A") then
                        Just (Just Left)

                    else if keyPressed Keyboard.ArrowRight || keyPressed (Keyboard.Character "D") then
                        Just (Just Right)

                    else if keyPressed Keyboard.ArrowUp || keyPressed (Keyboard.Character "W") then
                        Just (Just Up)

                    else if keyPressed Keyboard.ArrowDown || keyPressed (Keyboard.Character "S") then
                        Just (Just Down)

                    else if keyPressed Keyboard.Spacebar then
                        Just Nothing

                    else
                        Nothing

                maybeMoveAction2 =
                    case maybeMoveAction of
                        Just action_ ->
                            if LevelState.canMakeMove model.currentLevel model.timelineCache model.moveActions action_ then
                                Just action_

                            else
                                Nothing

                        Nothing ->
                            Nothing

                model_ =
                    case ( keyDown Keyboard.Control, keyPressed (Keyboard.Character "Z") ) of
                        ( True, True ) ->
                            setMoveActions
                                (model.moveActions |> List.reverse |> List.drop 1 |> List.reverse)
                                { model | targetTime = Nothing }
                                |> (\a -> { a | viewTime = getCurrentTime a a.timelineCache |> toFloat })

                        _ ->
                            case maybeMoveAction2 of
                                Just moveAction2 ->
                                    setMoveActions
                                        (model.moveActions ++ [ moveAction2 ])
                                        { model | targetTime = Nothing }

                                Nothing ->
                                    model

                maybeTimeAdjust =
                    if keyPressed (Keyboard.Character "Q") then
                        Just -1

                    else if keyPressed (Keyboard.Character "E") then
                        Just 1

                    else
                        Nothing
            in
            ( { model_
                | keys = newKeys
                , targetTime =
                    case maybeMoveAction2 of
                        Just _ ->
                            Nothing

                        Nothing ->
                            case ( maybeTimeAdjust, model_.targetTime ) of
                                ( Just timeAdjust, Just currentTime ) ->
                                    currentTime + timeAdjust |> Just

                                ( Just timeAdjust, Nothing ) ->
                                    LevelState.currentPlayerTime model_.timelineCache model_.moveActions + timeAdjust |> Just

                                ( Nothing, _ ) ->
                                    model_.targetTime
              }
            , Cmd.none
            )

        UrlClicked urlRequest ->
            ( model
            , case urlRequest of
                Browser.Internal url ->
                    Browser.Navigation.pushUrl model.navigationKey (Url.toString url)

                Browser.External url ->
                    Browser.Navigation.load url
            )

        UrlChanged url ->
            ( model, Cmd.none )

        PressedNextLevel ->
            ( case
                ( model.futureLevels
                , LevelState.isCompleted
                    model.currentLevel
                    (LevelState.timeline model.currentLevel model.moveActions)
                    model.moveActions
                )
              of
                ( head :: rest, True ) ->
                    { model | currentLevel = head, futureLevels = rest, viewTime = 0 }
                        |> setMoveActions []

                _ ->
                    model
            , Cmd.none
            )

        PressedSkipLevel ->
            ( case model.futureLevels of
                head :: rest ->
                    { model | currentLevel = head, futureLevels = rest, viewTime = 0 }
                        |> setMoveActions []

                _ ->
                    model
            , Cmd.none
            )

        PressedResetLevel ->
            ( setMoveActions [] { model | viewTime = 0 }
            , Cmd.none
            )

        DraggedTimelineSlider newTime ->
            ( { model | viewTime = newTime, targetTime = Just (round newTime) }, Cmd.none )

        SliderLostFocus ->
            ( model, Cmd.none )

        AnimationFrame time ->
            ( { model
                | time = time
                , viewTime =
                    let
                        stepSize : Float
                        stepSize =
                            0.15

                        targetTime : Float
                        targetTime =
                            case model.targetTime of
                                Just targetTime_ ->
                                    toFloat targetTime_

                                Nothing ->
                                    getCurrentTime model model.timelineCache |> toFloat
                    in
                    if targetTime > model.viewTime + stepSize then
                        model.viewTime + stepSize

                    else if targetTime < model.viewTime - stepSize then
                        model.viewTime - stepSize

                    else
                        targetTime
              }
            , Cmd.none
            )


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


viewLevel : Loaded_ -> Element msg
viewLevel model =
    let
        ( w, h ) =
            Level.levelSize model.currentLevel

        walls : Dict Point WallType
        walls =
            Level.getWalls model.currentLevel

        portals : List { timeDelta : Int, portal : Portal }
        portals =
            Level.portalPairs model.currentLevel
                |> List.concatMap
                    (\portalPair ->
                        [ { timeDelta = portalPair.timeDelta, portal = portalPair.firstPortal }
                        , { timeDelta = -portalPair.timeDelta, portal = portalPair.secondPortal }
                        ]
                    )

        paradoxes : List Paradox
        paradoxes =
            LevelState.paradoxes model.currentLevel model.timelineCache

        currentTimeInt =
            floor model.viewTime

        current : LevelInstant
        current =
            LevelState.getTimelineInstant model.currentLevel currentTimeInt model.timelineCache

        t =
            model.viewTime - toFloat currentTimeInt |> T

        doors : List DoorInstant
        doors =
            LevelState.doors model.currentLevel current

        exit =
            Level.exit model.currentLevel

        laserBeams : Set LaserBeam
        laserBeams =
            LevelState.getLaserBeams model.currentLevel model.timelineCache currentTimeInt

        lasers : List Level.Laser
        lasers =
            Level.lasers model.currentLevel
    in
    List.range 0 (w - 1)
        |> List.map
            (\x ->
                List.range 0 (h - 1)
                    |> List.map
                        (\y ->
                            let
                                position : Point
                                position =
                                    Point.new x y

                                localPortals : List ( Int, TileEdge )
                                localPortals =
                                    List.filterMap
                                        (\portal ->
                                            if portal.portal.position == position then
                                                Just ( portal.timeDelta, portal.portal.tileEdge )

                                            else
                                                Nothing
                                        )
                                        portals

                                borderWidth : TileEdge -> Int
                                borderWidth tileEdge =
                                    if
                                        List.any (Tuple.second >> (==) tileEdge) localPortals
                                            || (exit.position == position)
                                            && (exit.tileEdge == tileEdge)
                                    then
                                        6

                                    else
                                        0

                                tileParadoxes : List Paradox
                                tileParadoxes =
                                    List.filter (.position >> (==) position) paradoxes

                                maybeDoor =
                                    case List.find (\{ door } -> door.doorPosition == position) doors of
                                        Just { isOpen } ->
                                            Just isOpen

                                        Nothing ->
                                            Nothing
                            in
                            Element.el
                                (Element.width (Element.px tileSize)
                                    :: Element.height (Element.px tileSize)
                                    :: Element.Font.center
                                    :: Element.Border.width 1
                                    :: drawParadox currentTimeInt tileParadoxes
                                    :: (Element.el
                                            [ Element.Border.widthEach
                                                { left = borderWidth LeftEdge
                                                , right = borderWidth RightEdge
                                                , top = borderWidth TopEdge
                                                , bottom = borderWidth BottomEdge
                                                }
                                            , Element.Border.color
                                                (if exit.position == position then
                                                    if List.isEmpty paradoxes then
                                                        Element.rgb 0 0.8 0

                                                    else
                                                        Element.rgb 0.8 0 0

                                                 else
                                                    Element.rgb 0.3 0.3 1
                                                )
                                            , Element.width Element.fill
                                            , Element.height Element.fill
                                            ]
                                            Element.none
                                            |> Element.inFront
                                       )
                                    :: drawWallsAndDoorBackground position maybeDoor walls
                                    :: drawLaser position lasers
                                    ++ drawLaserBeam position laserBeams
                                )
                                (if List.any (\box -> box.position == position) current.boxes then
                                    Element.el
                                        [ Element.centerX
                                        , Element.centerY
                                        , Element.Font.size 30
                                        ]
                                        (Element.text "▨")

                                 else if maybeDoor == Just True then
                                    Element.column
                                        [ Element.Font.size 14
                                        , Element.centerY
                                        , Element.Font.center
                                        , Element.width Element.fill
                                        ]
                                        [ Element.el [ Element.centerX ] (Element.text "Open")
                                        , Element.el [ Element.centerX ] (Element.text "door")
                                        ]

                                 else if maybeDoor == Just False then
                                    Element.column
                                        [ Element.Font.size 14
                                        , Element.centerY
                                        , Element.width Element.fill
                                        , Element.Font.color (Element.rgb 0.8 0.8 0.8)
                                        ]
                                        [ Element.el [ Element.centerX ] (Element.text "Closed")
                                        , Element.el [ Element.centerX ] (Element.text "door")
                                        ]

                                 else
                                    case List.head localPortals of
                                        Just ( timeDelta, _ ) ->
                                            (if timeDelta < 0 then
                                                "t" ++ String.fromInt timeDelta

                                             else
                                                "t+" ++ String.fromInt timeDelta
                                            )
                                                |> Element.text
                                                |> Element.el
                                                    [ Element.Font.size 12
                                                    , Element.centerX
                                                    , Element.centerY
                                                    ]

                                        Nothing ->
                                            if Level.exit model.currentLevel |> .position |> (==) (Point.new x y) then
                                                Element.el
                                                    [ Element.centerX
                                                    , Element.centerY
                                                    , Element.Font.size 14
                                                    ]
                                                    (Element.text "Exit")

                                            else if
                                                Level.doors model.currentLevel
                                                    |> List.any (.buttonPosition >> (==) (Point.new x y))
                                            then
                                                Element.el
                                                    [ Element.centerX
                                                    , Element.centerY
                                                    , Element.Font.size 14
                                                    ]
                                                    (Element.text "B")

                                            else
                                                Element.none
                                )
                        )
                    |> Element.column []
            )
        |> Element.row
            (List.gatherEqualsBy .position current.players
                |> List.concatMap
                    (\( { position, age }, rest ) ->
                        drawPlayers t position (Nonempty age (List.map .age rest)) model
                    )
            )


tileSize : Int
tileSize =
    50


type T
    = T Float


toFrom : Float -> Float -> T -> Float
toFrom start end (T t) =
    (end - start) * t + start


tToFloat : T -> Float
tToFloat (T t) =
    t


drawPlayers : T -> Point -> Nonempty Int -> Loaded_ -> List (Element.Attribute msg)
drawPlayers t ( x, y ) ages model =
    if t == T 0 then
        [ Element.row
            [ (x * tileSize + 16) |> toFloat |> Element.moveRight
            , (y * tileSize + 15) |> toFloat |> Element.moveDown
            ]
            [ Element.el
                [ if List.Nonempty.any ((==) (List.length model.moveActions)) ages then
                    Element.Font.bold

                  else
                    Element.Font.regular
                ]
                (Element.text "P")
            , List.Nonempty.toList ages
                |> List.map String.fromInt
                |> String.join "&"
                |> Element.text
                |> Element.el [ Element.Font.size 12, Element.moveDown 6 ]
            ]
            |> Element.inFront
        ]

    else
        List.Nonempty.toList ages
            |> List.map
                (\age ->
                    let
                        ( moveX, moveY ) =
                            List.getAt age model.moveActions
                                |> Maybe.withDefault Nothing
                                |> LevelState.directionOffset
                                |> Point.scale tileSize
                    in
                    Element.row
                        [ (x * tileSize + 16)
                            |> toFloat
                            |> (+) (toFrom 0 (toFloat moveX) t)
                            |> Element.moveRight
                        , (y * tileSize + 15)
                            |> toFloat
                            |> (+) (toFrom 0 (toFloat moveY) t)
                            |> Element.moveDown
                        ]
                        [ Element.el
                            [ if List.Nonempty.any ((==) (List.length model.moveActions)) ages then
                                Element.Font.bold

                              else
                                Element.Font.regular
                            ]
                            (Element.text "P")
                        , String.removeTrailing0s 1 (toFloat age + tToFloat t)
                            |> Element.text
                            |> Element.el [ Element.Font.size 12, Element.moveDown 6 ]
                        ]
                        |> Element.inFront
                )


drawLaser : Point -> List Laser -> List (Element.Attribute msg)
drawLaser position lasers =
    List.filterMap
        (\laser ->
            if laser.position == position then
                Element.el
                    (Element.Background.color (Element.rgb 0.5 0.2 0.2)
                        :: Element.width (Element.px 12)
                        :: Element.height (Element.px 12)
                        :: (case laser.tileEdge of
                                LeftEdge ->
                                    [ Element.centerY ]

                                TopEdge ->
                                    [ Element.centerX ]

                                BottomEdge ->
                                    [ Element.centerX, Element.alignBottom ]

                                RightEdge ->
                                    [ Element.centerY, Element.alignRight ]
                           )
                    )
                    Element.none
                    |> Element.inFront
                    |> Just

            else
                Nothing
        )
        lasers


drawParadox : Int -> List Paradox -> Element.Attribute msg
drawParadox currentTime tileParadoxes =
    (if List.isEmpty tileParadoxes then
        Element.none

     else
        Element.el
            [ Element.Font.size 14
            , (if List.any (.time >> (==) currentTime) tileParadoxes then
                1

               else
                0.4
              )
                |> Element.alpha
            , Element.Font.bold
            , Element.alignTop
            , Element.alignRight
            , Element.Font.color (Element.rgb 0.8 0 0)
            ]
            (Element.text "⚠")
    )
        |> Element.inFront


drawWallsAndDoorBackground position maybeDoor walls =
    case Dict.get position walls of
        Just Wall ->
            Element.Background.color (Element.rgb 0 0 0)

        Just Glass ->
            Element.Background.color (Element.rgb 0.9 0.9 0.8)

        Nothing ->
            case maybeDoor of
                Just True ->
                    Element.Background.color (Element.rgb 0.8 0.8 0.8)

                Just False ->
                    Element.Background.color (Element.rgb 0.4 0.4 0.4)

                Nothing ->
                    Element.Background.color (Element.rgb 1 1 1)


drawLaserBeam : Point -> Set LaserBeam -> List (Element.Attribute msg)
drawLaserBeam position lasers =
    [ (if Set.member { position = position, isVertical = True } lasers then
        Element.el
            [ Element.width (Element.px 4)
            , Element.height Element.fill
            , Element.centerX
            , Element.Background.color (Element.rgb 1 0 0)
            ]
            Element.none

       else
        Element.none
      )
        |> Element.behindContent
    , (if Set.member { position = position, isVertical = False } lasers then
        Element.el
            [ Element.height (Element.px 4)
            , Element.width Element.fill
            , Element.centerY
            , Element.Background.color (Element.rgb 1 0 0)
            ]
            Element.none

       else
        Element.none
      )
        |> Element.behindContent
    ]


getCurrentTime :
    Loaded_
    -> RegularDict.Dict Int LevelInstant
    -> Int
getCurrentTime model timeline =
    case model.targetTime of
        Just currentTime_ ->
            currentTime_

        Nothing ->
            LevelState.currentPlayerTime timeline model.moveActions


viewLoaded : Loaded_ -> Element FrontendMsg
viewLoaded model =
    let
        paradoxes : List Paradox
        paradoxes =
            LevelState.paradoxes model.currentLevel model.timelineCache
    in
    Element.column
        [ Element.padding 16, Element.spacing 8, Element.width Element.fill ]
        [ viewLevel model
        , slider model.viewTime (LevelState.currentPlayerTime model.timelineCache model.moveActions) paradoxes model.timelineCache
        , if LevelState.isCompleted model.currentLevel model.timelineCache model.moveActions then
            Element.Keyed.row
                [ Element.spacing 16 ]
                [ ( "a", Element.el [ Element.Font.color (Element.rgb 0 0.8 0) ] (Element.text "Level complete!") )
                , ( "b"
                  , if List.isEmpty model.futureLevels then
                        Element.text "No more levels :("

                    else
                        button buttonAttributes { onPress = PressedNextLevel, label = Element.text "Next level" }
                  )
                ]

          else if List.isEmpty model.futureLevels then
            Element.none

          else
            Element.Keyed.row [ Element.spacing 16 ]
                [ ( "c", button buttonAttributes { onPress = PressedResetLevel, label = Element.text "Reset level" } )
                , ( "d", button buttonAttributes { onPress = PressedSkipLevel, label = Element.text "Skip level" } )
                ]
        , if List.isEmpty paradoxes then
            Element.none

          else
            ("Paradox at t=" ++ String.join "&" (List.map (.time >> String.fromInt) paradoxes))
                |> Element.text
                |> Element.el [ Element.Font.color (Element.rgb 1 0 0) ]
        , Element.column
            []
            [ Element.paragraph []
                [ Element.text "You control the "
                , Element.el [ Element.Font.bold ] (Element.text "P")
                , Element.text " character. Move with arrow keys or WASD. Press space to wait 1 turn."
                ]
            ]
        , Element.column
            []
            [ Element.paragraph [] [ Element.text "Undo moves with ctrl+z" ] ]
        , Element.column
            []
            [ Element.paragraph [] [ Element.text "B = button that opens door" ] ]
        , Element.column
            []
            [ Element.paragraph [] [ Element.text "t-n = A portal that moves you n seconds into the past" ] ]
        , Element.column
            []
            [ Element.paragraph [] [ Element.text "t+n = A portal that moves you n seconds into the future" ] ]
        , Element.column
            []
            [ Element.paragraph [] [ Element.text "You can only use the exit when there are no paradoxes" ] ]
        ]


slider : Float -> Int -> List Paradox -> RegularDict.Dict Int LevelInstant -> Element FrontendMsg
slider viewTime playerTime paradoxes timeline =
    let
        minTime =
            RegularDict.keys timeline
                |> List.minimum
                |> Maybe.withDefault 0
                |> (\a -> 5 * ((a - 4) // 5))

        maxTime =
            RegularDict.keys timeline
                |> List.maximum
                |> Maybe.withDefault 0
                |> max (minTime + 1)
                |> (\a -> 5 + 5 * (a // 5))
    in
    Element.Keyed.column
        [ Element.width <| Element.maximum 900 (Element.px (70 * (maxTime - minTime))), Element.paddingXY 0 8 ]
        [ ( String.fromInt playerTime
          , Element.Input.slider
                [ Element.width Element.fill
                , Element.height (Element.px 12)
                , List.range minTime (maxTime - 1)
                    |> List.map
                        (\time ->
                            Element.el
                                [ Element.width Element.fill
                                , Element.height Element.fill
                                , Element.Font.size 14
                                , Element.Border.widthEach { left = 1, right = 1, top = 1, bottom = 1 }
                                , (if List.any (.time >> (==) time) paradoxes then
                                    if viewTime >= toFloat time && viewTime < toFloat time + 1 then
                                        Element.rgb 1 0.1 0.1

                                    else
                                        Element.rgb 0.8 0 0

                                   else if viewTime >= toFloat time && viewTime < toFloat time + 1 then
                                    Element.rgb 0.7 0.7 0.7

                                   else
                                    Element.rgb 0.5 0.5 0.5
                                  )
                                    |> Element.Background.color
                                , Element.above
                                    (if minTime == time || maxTime == time || playerTime == time || modBy 5 time == 0 then
                                        Element.el [ Element.moveRight 8 ] (Element.text (String.fromInt time))

                                     else
                                        Element.none
                                    )
                                ]
                                (if playerTime == time then
                                    Element.el [ Element.moveRight 8 ] (Element.text "You")

                                 else
                                    Element.none
                                )
                        )
                    |> Element.row
                        [ Element.width Element.fill, Element.height Element.fill, Element.paddingXY 5 0 ]
                    |> Element.behindContent
                ]
                { onChange = DraggedTimelineSlider
                , noOp = SliderLostFocus
                , label = Element.Input.labelLeft [] (Element.text "Timeline")
                , min = toFloat minTime
                , max = toFloat maxTime
                , value = clamp (toFloat minTime) (toFloat maxTime) viewTime
                , thumb =
                    Element.Input.thumb
                        [ Element.width (Element.px 10)
                        , Element.Border.rounded 4
                        , Element.height (Element.px 24)
                        , Element.Background.color (Element.rgb 0.9 0.9 0.9)
                        , Element.Border.width 1
                        , Element.Border.color (Element.rgb 0.3 0.3 0.3)
                        ]
                , step = Just 0.01
                }
          )
        ]


buttonAttributes =
    [ Element.padding 8
    , Element.Background.color (Element.rgb 0.7 0.7 0.7)
    , Element.Font.center
    ]


button : List (Element.Attribute msg) -> { b | onPress : msg, label : Element msg } -> Element msg
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
        , Browser.Events.onAnimationFrame AnimationFrame
        ]
