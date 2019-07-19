module Point exposing (Point, add, new, scale)


type alias Point =
    ( Int, Int )


new : Int -> Int -> Point
new x y =
    ( x, y )


add : Point -> Point -> Point
add ( x0, y0 ) ( x1, y1 ) =
    ( x0 + x1, y0 + y1 )


scale : Int -> Point -> Point
scale amount ( x, y ) =
    ( x * amount, y * amount )
