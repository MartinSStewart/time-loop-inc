module AsciiRender exposing (getRows, getText, view)

import Array exposing (Array)
import Array2D exposing (Array2D)
import Basics.Extra exposing (flip)
import Dict
import Level exposing (Level, TileEdge(..))
import LevelState exposing (LevelInstant)
import List.Extra as List
import Parser
import Point exposing (Point)
import Set


wallChar =
    '█'


boxChar =
    '□'


exitChar =
    'E'


playerPrimeChar =
    'P'


playerCloneChar =
    'p'


emptyChar =
    '.'


view : Level -> LevelInstant -> Int -> String
view level timeState time =
    let
        ( width, height ) =
            level.levelSize

        ( playerX, playerY ) =
            Tuple.first timeState.playerPrime

        ( exitX, exitY ) =
            level.exit

        emptyGrid =
            Array2D.initialize height width (\_ _ -> emptyChar)
                |> Array2D.set exitY exitX exitChar

        gridWithWalls =
            List.foldl (\( x, y ) grid -> Array2D.set y x wallChar grid) emptyGrid (Set.toList level.walls)

        gridWithBoxes =
            List.foldl (\( x, y ) grid -> Array2D.set y x boxChar grid) gridWithWalls timeState.boxes

        gridWithPlayers =
            timeState.players
                |> List.map Tuple.first
                |> List.foldl (\( x, y ) grid -> Array2D.set y x playerCloneChar grid) gridWithBoxes

        gridWithPlayerPrime =
            Array2D.set playerY playerX playerPrimeChar gridWithPlayers

        scaledGrid =
            Array2D.initialize
                (height * 3)
                (width * 3)
                (\row col ->
                    case ( modBy 3 row, modBy 3 col ) of
                        ( 0, 0 ) ->
                            '┌'

                        ( 0, 1 ) ->
                            '─'

                        ( 0, 2 ) ->
                            '┐'

                        ( 1, 0 ) ->
                            '│'

                        ( 1, 1 ) ->
                            Array2D.get (row // 3) (col // 3) gridWithPlayerPrime |> Maybe.withDefault '.'

                        ( 1, 2 ) ->
                            '│'

                        ( 2, 0 ) ->
                            '└'

                        ( 2, 1 ) ->
                            '─'

                        ( 2, 2 ) ->
                            '┘'

                        _ ->
                            '?'
                )

        gridWithPortals =
            level.portalPairs
                |> List.indexedMap Tuple.pair
                |> List.foldl
                    (\( index, portalPair ) grid ->
                        let
                            ( x0, y0 ) =
                                portalPair.firstPortal.position
                                    |> Point.scale 3
                                    |> Point.add ( 1, 1 )
                                    |> Point.add (offset portalPair.firstPortal)

                            ( x1, y1 ) =
                                portalPair.secondPortal.position
                                    |> Point.scale 3
                                    |> Point.add ( 1, 1 )
                                    |> Point.add (offset portalPair.secondPortal)

                            offset portal =
                                case portal.tileEdge of
                                    LeftEdge ->
                                        ( -1, 0 )

                                    RightEdge ->
                                        ( 1, 0 )

                                    TopEdge ->
                                        ( 0, -1 )

                                    BottomEdge ->
                                        ( 0, 1 )

                            ( entrance, exit ) =
                                getPortalChar index
                        in
                        grid
                            |> Array2D.set y0 x0 entrance
                            |> Array2D.set y1 x1 exit
                    )
                    scaledGrid

        portalInfoLines =
            level.portalPairs
                |> List.indexedMap
                    (\index portalPair ->
                        let
                            ( entrance, exit ) =
                                getPortalChar index
                        in
                        "\n"
                            ++ String.fromChar entrance
                            ++ " -> "
                            ++ String.fromChar exit
                            ++ ": "
                            ++ String.fromInt portalPair.timeDelta
                            ++ "\n"
                    )
                |> String.concat
    in
    gridWithPortals
        |> getText
        |> flip (++) ("\nTime: " ++ String.fromInt time ++ "\n")
        |> flip (++) portalInfoLines


getText : Array2D Char -> String
getText array2d =
    getRows array2d
        |> List.map (Array.toList >> String.fromList >> flip String.append "\n")
        |> String.concat


getRows : Array2D a -> List (Array a)
getRows array2d =
    List.range 0 (Array2D.rows array2d - 1)
        |> List.map (flip Array2D.getRow array2d)
        |> List.filterMap identity


getPortalChar : Int -> ( Char, Char )
getPortalChar index =
    let
        entranceChar =
            index + Char.toCode 'A' |> Char.fromCode
    in
    ( entranceChar, Char.toLower entranceChar )


parse : String -> Result String Level
parse levelText =
    let
        levelLines =
            levelText |> String.trim |> String.lines |> List.map String.trim
    in
    case levelLines |> List.head |> Maybe.map String.length of
        Just levelCharWidth ->
            if modBy 3 levelCharWidth == 0 then
                let
                    levelWidth =
                        levelCharWidth // 3

                    levelRows : List ( Point, RowItem )
                    levelRows =
                        List.groupsOf 3 levelLines
                            |> List.indexedMap
                                (\index rows ->
                                    case rows of
                                        [ top, middle, bottom ] ->
                                            parseRow index top middle bottom |> Ok

                                        _ ->
                                            Err "Wrong number of rows to parse."
                                )
                            |> List.takeWhile
                                (\value ->
                                    case value of
                                        Ok _ ->
                                            True

                                        Err _ ->
                                            False
                                )

                    levelItems =
                        levelRows
                            |> List.filterMap
                                (\value ->
                                    case value of
                                        Ok ok ->
                                            Just ok

                                        Err _ ->
                                            Nothing
                                )
                            |> List.concat

                    exit =
                        levelItems |> List.filter (Tuple.second >> (==) Wall) |> List.map Tuple.first |> List.head

                    playerStart =
                        levelItems |> List.filter (Tuple.second >> (==) Player) |> List.map Tuple.first |> List.head

                    portals =
                        levelItems
                            |> List.filterMap
                                (\( position, item ) ->
                                    case item of
                                        Portal portal ->
                                            Just ( position, Portal portal )

                                        _ ->
                                            Nothing
                                )
                            |> List.gatherWith (\( _, portal ) -> portal.id)
                            |> List.filterMap
                                (\( _, portals ) ->
                                    case portals of
                                        [ first, second ] ->
                                            let
                                                portalFirst =
                                                    { position = Tuple.first first
                                                    , tileEdge = (Tuple.second first).edge
                                                    }

                                                portalSecond =
                                                    { position = Tuple.first second
                                                    , tileEdge = (Tuple.second second).edge
                                                    }
                                            in
                                            Level.PortalPair

                                        _ ->
                                            Nothing
                                )
                in
                case ( exit, playerStart ) of
                    ( Just exit_, Just player_ ) ->
                        { playerStart = player_
                        , walls = levelItems |> List.filter (Tuple.second >> (==) Wall) |> List.map Tuple.first |> Set.fromList
                        , boxesStart = levelItems |> List.filter (Tuple.second >> (==) Wall) |> List.map Tuple.first |> Set.fromList
                        , exit = exit_
                        , levelSize = Point.new levelWidth (List.length levelRows)
                        , portalPairs = List PortalPair
                        }

            else
                Err "Wrong character width for level."

        Nothing ->
            Err "Failed to determine level width."


type RowItem
    = Portal { id : Int, isEntrance : Bool, edge : TileEdge }
    | Wall
    | Player
    | Box
    | Empty


parseRow : Int -> String -> String -> String -> List ( Point, RowItem )
parseRow rowNumber topCharRow middleCharRow bottomCharRow =
    let
        getRowItem : Maybe TileEdge -> Char -> Maybe RowItem
        getRowItem maybeTileEdge char =
            case maybeTileEdge of
                Nothing ->
                    if char == wallChar then
                        Just Wall

                    else if char == playerPrimeChar then
                        Just Player

                    else if char == boxChar then
                        Just Box

                    else if char == emptyChar then
                        Just Empty

                    else
                        Nothing

                Just tileEdge ->
                    if Char.isAlpha char then
                        if Char.isUpper char then
                            { id = Char.toCode char - Char.toCode 'A', isEntrance = True, edge = tileEdge } |> Portal |> Just

                        else
                            { id = Char.toCode char - Char.toCode 'a', isEntrance = False, edge = tileEdge } |> Portal |> Just

                    else
                        Nothing

        topRow =
            topCharRow
                |> String.toList
                |> List.drop 1
                |> List.groupsOfWithStep 1 2
                |> List.map (List.head >> Maybe.withDefault '?' >> getRowItem (Just Level.TopEdge))
                |> List.indexedMap (\index value -> ( Point.new rowNumber index, value ))

        middleRow =
            middleCharRow
                |> String.toList
                |> List.indexedMap
                    (\index value ->
                        let
                            rowItem =
                                case modBy 3 index of
                                    0 ->
                                        getRowItem (Just Level.LeftEdge) value

                                    1 ->
                                        getRowItem Nothing value

                                    _ ->
                                        getRowItem (Just Level.RightEdge) value
                        in
                        ( Point.new rowNumber (modBy 3 index), rowItem )
                    )

        bottomRow =
            bottomCharRow
                |> String.toList
                |> List.drop 1
                |> List.groupsOfWithStep 1 2
                |> List.map (List.head >> Maybe.withDefault '?' >> getRowItem (Just Level.BottomEdge))
                |> List.indexedMap (\index value -> ( Point.new rowNumber index, value ))

        allItems =
            topRow ++ middleRow ++ bottomRow
    in
    allItems
        |> List.filterMap
            (\( position, rowItem ) ->
                case rowItem of
                    Just rowItem_ ->
                        Just ( position, rowItem_ )

                    Nothing ->
                        Nothing
            )
