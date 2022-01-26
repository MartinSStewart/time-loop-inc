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
import LevelState exposing (DoorInstant, LevelInstant, MoveAction(..), Paradox)
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
--getLevel : Result String Level
--getLevel =
--    Level.init
--        { playerStart = ( 1, 1 )
--        , walls = [ ( 0, 0 ), ( 0, 1 ) ] |> Set.fromList
--        , boxesStart = [ ( 2, 1 ) ] |> Set.fromList
--        , exit =
--            { position = ( 3, 0 )
--            , tileEdge = TopEdge
--            }
--        , levelSize = ( 5, 5 )
--        , portalPairs =
--            [ { firstPortal = { position = ( 0, 2 ), tileEdge = LeftEdge }
--              , secondPortal = { position = ( 4, 3 ), tileEdge = RightEdge }
--              , timeDelta = 2
--              }
--            , { firstPortal = { position = ( 0, 4 ), tileEdge = LeftEdge }
--              , secondPortal = { position = ( 4, 1 ), tileEdge = RightEdge }
--              , timeDelta = 8
--              }
--            ]
--        , doors = [ { doorPosition = ( 2, 2 ), buttonPosition = ( 2, 3 ) } ]
--        }


level0 : Result String Level
level0 =
    Level.init
        { playerStart = ( 1, 2 )
        , walls = [ ( 3, 0 ), ( 3, 1 ), ( 3, 3 ), ( 3, 4 ) ] |> Set.fromList
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
        }


init : Url -> Browser.Navigation.Key -> ( FrontendModel, Cmd FrontendMsg )
init _ navigationKey =
    ( { navigationKey = navigationKey } |> initLoaded
    , Cmd.none
    )


initLoaded : Loading_ -> FrontendModel
initLoaded loading =
    case level0 of
        Ok level ->
            { navigationKey = loading.navigationKey
            , moveActions = []
            , currentTime = Nothing
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
                                | moveActions =
                                    model.moveActions |> List.reverse |> List.drop 1 |> List.reverse
                                , currentTime = Nothing
                            }

                        _ ->
                            model

                timeline =
                    LevelState.timeline model.level model.moveActions

                action =
                    if LevelState.isCompleted model.level timeline model.moveActions then
                        Nothing

                    else if keyPressed Keyboard.ArrowLeft then
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

                action2 =
                    case action of
                        Just action_ ->
                            if LevelState.canMakeMove model.level timeline model.moveActions action_ then
                                Just action_

                            else
                                Nothing

                        Nothing ->
                            Nothing
            in
            ( { model_
                | keys = newKeys
                , moveActions = model_.moveActions ++ Maybe.toList action2
                , currentTime =
                    case action2 of
                        Just _ ->
                            Nothing

                        Nothing ->
                            model_.currentTime
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

        PressedTimeMinus ->
            let
                currentTime =
                    getCurrentTime model (LevelState.timeline model.level model.moveActions)
            in
            ( { model | currentTime = currentTime - 1 |> Just }, Cmd.none )

        PressedTimePlus ->
            let
                currentTime =
                    getCurrentTime model (LevelState.timeline model.level model.moveActions)
            in
            ( { model | currentTime = currentTime + 1 |> Just }, Cmd.none )


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


viewLevel : Level -> RegularDict.Dict Int LevelInstant -> Int -> Element msg
viewLevel level timeline currentTime =
    let
        ( w, h ) =
            Level.levelSize level

        walls : Set Point
        walls =
            Level.getWalls level

        portals : List { timeDelta : Int, portal : Portal }
        portals =
            Level.portalPairs level
                |> List.concatMap
                    (\portalPair ->
                        [ { timeDelta = portalPair.timeDelta, portal = portalPair.firstPortal }
                        , { timeDelta = -portalPair.timeDelta, portal = portalPair.secondPortal }
                        ]
                    )

        paradoxes : List Paradox
        paradoxes =
            LevelState.paradoxes level timeline

        current : LevelInstant
        current =
            LevelState.getTimelineInstant level currentTime timeline

        doors : List DoorInstant
        doors =
            LevelState.doors level current

        exit =
            Level.exit level
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
                            in
                            Element.el
                                [ Element.width (Element.px 50)
                                , Element.height (Element.px 50)
                                , Element.Font.center
                                , Element.Border.width 1
                                , (if List.isEmpty tileParadoxes then
                                    Element.none

                                   else
                                    Element.el
                                        [ Element.Font.size 14
                                        , (if List.any (.time >> (==) currentTime) tileParadoxes then
                                            1

                                           else
                                            0.3
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
                                , Element.el
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
                                , if Set.member position walls then
                                    Element.Background.color (Element.rgb 0 0 0)

                                  else
                                    case List.find (\{ door } -> door.doorPosition == position) doors of
                                        Just { isOpen } ->
                                            if isOpen then
                                                Element.Background.color (Element.rgb 0.8 0.8 0.8)

                                            else
                                                Element.Background.color (Element.rgb 0.4 0.4 0.4)

                                        Nothing ->
                                            Element.Background.color (Element.rgb 1 1 1)
                                ]
                                (case List.filter (\player -> player.position == position) current.players of
                                    [] ->
                                        if List.any (\box -> box.position == position) current.boxes then
                                            Element.el [ Element.centerX, Element.centerY ] (Element.text "▨")

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
                                                    if Level.exit level |> .position |> (==) (Point.new x y) then
                                                        Element.el
                                                            [ Element.centerX
                                                            , Element.centerY
                                                            , Element.Font.size 14
                                                            ]
                                                            (Element.text "Exit")

                                                    else if Level.doors level |> List.any (.buttonPosition >> (==) (Point.new x y)) then
                                                        Element.el
                                                            [ Element.centerX
                                                            , Element.centerY
                                                            , Element.Font.size 14
                                                            ]
                                                            (Element.text "B")

                                                    else
                                                        Element.none

                                    players ->
                                        Element.row
                                            [ Element.centerX, Element.centerY ]
                                            [ Element.text "P"
                                            , List.map (.age >> String.fromInt) players
                                                |> String.join "&"
                                                |> Element.text
                                                |> Element.el [ Element.Font.size 12, Element.moveDown 6 ]
                                            ]
                                )
                        )
                    |> Element.column []
            )
        |> Element.row []


getCurrentTime :
    Loaded_
    -> RegularDict.Dict Int LevelInstant
    -> Int
getCurrentTime model timeline =
    case model.currentTime of
        Just currentTime_ ->
            currentTime_

        Nothing ->
            LevelState.currentPlayerTime timeline model.moveActions


viewLoaded : Loaded_ -> Element FrontendMsg
viewLoaded model =
    let
        timeline =
            LevelState.timeline model.level model.moveActions

        currentTime =
            getCurrentTime model timeline

        paradoxes : List Paradox
        paradoxes =
            LevelState.paradoxes model.level timeline

        --_ =
        --    Debug.log "" model.moveActions
    in
    Element.column
        [ Element.padding 16, Element.spacing 8 ]
        [ viewLevel model.level timeline currentTime
        , Element.row
            [ Element.spacing 64 ]
            [ Element.row [ Element.spacing 8, Element.width (Element.px 196) ]
                [ button
                    [ Element.padding 8
                    , Element.Background.color (Element.rgb 0.7 0.7 0.7)
                    , Element.width (Element.px 30)
                    , Element.Font.center
                    ]
                    { onPress = PressedTimeMinus, label = Element.text "-" }
                , Element.text ("Viewing t = " ++ String.fromInt currentTime)
                , button
                    [ Element.padding 8
                    , Element.Background.color (Element.rgb 0.7 0.7 0.7)
                    , Element.alignRight
                    , Element.width (Element.px 30)
                    , Element.Font.center
                    ]
                    { onPress = PressedTimePlus, label = Element.text "+" }
                ]
            , "You are at t = "
                ++ String.fromInt (LevelState.currentPlayerTime timeline model.moveActions)
                |> Element.text
            ]
        , if LevelState.isCompleted model.level timeline model.moveActions then
            Element.el [ Element.Font.color (Element.rgb 0 0.8 0) ] (Element.text "Level complete!")

          else if List.isEmpty paradoxes then
            Element.none

          else
            ("Paradox at t=" ++ String.join "&" (List.map (.time >> String.fromInt) paradoxes))
                |> Element.text
                |> Element.el [ Element.Font.color (Element.rgb 1 0 0) ]
        , Element.column
            []
            [ Element.paragraph [] [ Element.text "You control the P character. Move with arrow keys. Press space to wait 1 turn." ] ]
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
            [ Element.paragraph [] [ Element.text "t+n = A portal moves you back n seconds into the future" ] ]
        , Element.column
            []
            [ Element.paragraph [] [ Element.text "You can only use the exit when there are no paradoxes" ] ]
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
