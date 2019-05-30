module AsciiRender exposing (getRows, getText, portal0Entrance, portal1Entrance, view)

import Array exposing (Array)
import Array2D exposing (Array2D)
import Basics.Extra exposing (flip)
import Level exposing (Level, TileEdge(..))
import LevelState exposing (TimeState)
import Point
import Set


view : Level -> TimeState -> Int -> String
view level timeState time =
    let
        ( width, height ) =
            level.levelSize

        ( playerX, playerY ) =
            timeState.playerPrime

        ( exitX, exitY ) =
            level.exit

        emptyGrid =
            Array2D.initialize height width (\_ _ -> ' ')
                |> Array2D.set exitY exitX 'E'

        gridWithWalls =
            List.foldl (\( x, y ) grid -> Array2D.set y x '■' grid) emptyGrid (Set.toList level.walls)

        gridWithBoxes =
            List.foldl (\( x, y ) grid -> Array2D.set y x '□' grid) gridWithWalls timeState.boxes

        gridWithPlayers =
            List.foldl (\( x, y ) grid -> Array2D.set y x 'p' grid) gridWithBoxes timeState.players

        gridWithPlayerPrime =
            Array2D.set playerY playerX 'P' gridWithPlayers

        scaledGrid =
            Array2D.initialize
                (height * 2 + 1)
                (width * 2 + 1)
                (\row col ->
                    if modBy 2 row == 1 && modBy 2 col == 1 then
                        Array2D.get (row // 2) (col // 2) gridWithPlayerPrime |> Maybe.withDefault '.'

                    else
                        '.'
                )

        gridWithPortals =
            level.portalPairs
                |> List.indexedMap Tuple.pair
                |> List.foldl
                    (\( index, portalPair ) grid ->
                        let
                            ( x0, y0 ) =
                                portalPair.firstPortal.position
                                    |> Point.scale 2
                                    |> Point.add ( 1, 1 )
                                    |> Point.add (offset portalPair.firstPortal)

                            ( x1, y1 ) =
                                portalPair.secondPortal.position
                                    |> Point.scale 2
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

                            ( entranceChar, exitChar ) =
                                getPortalPairChar portalPair index
                        in
                        grid
                            |> Array2D.set y0 x0 entranceChar
                            |> Array2D.set y1 x1 exitChar
                    )
                    scaledGrid

        portalInfoLines =
            level.portalPairs
                |> List.indexedMap
                    (\index portalPair ->
                        let
                            ( entranceChar, exitChar ) =
                                getPortalPairChar portalPair index
                        in
                        String.fromChar entranceChar
                            ++ " -> "
                            ++ String.fromChar exitChar
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


getPortalChar portal chars =
    case portal.tileEdge of
        LeftEdge ->
            chars.left

        RightEdge ->
            chars.right

        TopEdge ->
            chars.up

        BottomEdge ->
            chars.down


getPortalPairChar portalPair index =
    (case index of
        0 ->
            ( portal0Entrance, portal0Exit )

        1 ->
            ( portal1Entrance, portal1Exit )

        _ ->
            ( portalInvalid, portalInvalid )
    )
        |> Tuple.mapBoth
            (getPortalChar portalPair.firstPortal)
            (getPortalChar portalPair.secondPortal)


portal0Entrance =
    { left = '┤'
    , right = '├'
    , up = '┴'
    , down = '┬'
    }


portal0Exit =
    { left = '╡'
    , right = '╞'
    , up = '╨'
    , down = '╥'
    }


portal1Entrance =
    { left = '╢'
    , right = '╟'
    , up = '╧'
    , down = '╤'
    }


portal1Exit =
    { left = '╣'
    , right = '╠'
    , up = '╩'
    , down = '╦'
    }


portalInvalid =
    { left = '?'
    , right = '?'
    , up = '?'
    , down = '?'
    }
