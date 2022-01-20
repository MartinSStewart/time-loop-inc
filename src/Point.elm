module Point exposing (Point, add, clamp, negate, new, scale)


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


clamp : Point -> Point -> Point -> Point
clamp ( minX, minY ) ( maxX, maxY ) ( x, y ) =
    new (Basics.clamp minX maxX x) (Basics.clamp minY maxY y)


negate : Point -> Point
negate ( x, y ) =
    new -x -y
