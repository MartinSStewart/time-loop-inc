module AsciiRender exposing (getRows, getText, view)

import Array exposing (Array)
import Array2D exposing (Array2D)
import Basics.Extra exposing (flip)
import Level exposing (Level, TileEdge(..))
import LevelState exposing (LevelInstant)
import Point
import Set


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
            Array2D.initialize height width (\_ _ -> ' ')
                |> Array2D.set exitY exitX 'E'

        gridWithWalls =
            List.foldl (\( x, y ) grid -> Array2D.set y x '█' grid) emptyGrid (Set.toList level.walls)

        gridWithBoxes =
            List.foldl (\( x, y ) grid -> Array2D.set y x '□' grid) gridWithWalls timeState.boxes

        gridWithPlayers =
            timeState.players
                |> List.map Tuple.first
                |> List.foldl (\( x, y ) grid -> Array2D.set y x 'p' grid) gridWithBoxes

        gridWithPlayerPrime =
            Array2D.set playerY playerX 'P' gridWithPlayers

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
            index + 65 |> Char.fromCode
    in
    ( entranceChar, Char.toLower entranceChar )
