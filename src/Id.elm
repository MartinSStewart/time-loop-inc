module Id exposing (Id(..), fromInt, toInt, toString)


type Id a
    = Id Int


toString : Id a -> String
toString (Id id) =
    String.fromInt id


fromInt : Int -> Id a
fromInt =
    Id


toInt : Id a -> Int
toInt (Id id) =
    id
