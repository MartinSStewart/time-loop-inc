module Point exposing (Point, add, scale)


type alias Point =
    ( Int, Int )


add : Point -> Point -> Point
add ( x0, y0 ) ( x1, y1 ) =
    ( x0 + x1, y0 + y1 )


scale : Int -> Point -> Point
scale amount ( x, y ) =
    ( x * amount, y * amount )
