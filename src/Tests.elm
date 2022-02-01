module Tests exposing (..)

import AssocSet as Set
import Dict as RegularDict
import Element exposing (Element)
import Element.Font
import Frontend
import Html exposing (Html)
import Level exposing (Level, TileEdge(..))
import LevelState exposing (LevelInstant, MoveAction(..))


main : Html ()
main =
    List.map
        viewTestResult
        [ test "Can push box" test0
        , test "Portal timing is correct" test4
        , test "Can push box back in time" test3
        , test "Go through portal and backwards in time" test1
        , test "Go through portal and forwards in time" test2
        ]
        |> Element.column [ Element.padding 8, Element.spacing 16 ]
        |> Element.layout []


viewTestResult : Test -> Element ()
viewTestResult (Test name testResult) =
    case testResult of
        Passed element ->
            Element.column
                [ Element.spacing 4, Element.width Element.fill ]
                [ Element.el
                    [ Element.Font.color (Element.rgb 0 0.8 0) ]
                    (Element.text ("Passed: " ++ name))
                , element
                ]

        Failed element ->
            Element.column
                [ Element.spacing 4, Element.width Element.fill ]
                [ Element.el
                    [ Element.Font.color (Element.rgb 1 0 0) ]
                    (Element.text ("Failed: " ++ name))
                , element
                ]


test : String -> (() -> TestResult) -> Test
test name testResult =
    Test name (testResult ())


type TestResult
    = Passed (Element ())
    | Failed (Element ())


type Test
    = Test String TestResult


test0 : () -> TestResult
test0 () =
    let
        moveActions =
            [ Just MoveRight ]

        level =
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
                , doors = []
                }
                |> unwrapResult

        output =
            LevelState.timeline level moveActions

        expected =
            [ ( 1, { boxes = [ { position = ( 3, 1 ) } ], players = [ { age = 1, position = ( 2, 1 ) } ] } )
            , ( 0, { boxes = [ { position = ( 2, 1 ) } ], players = [ { age = 0, position = ( 1, 1 ) } ] } )
            ]
                |> RegularDict.fromList
    in
    (if output == expected then
        Passed

     else
        Failed
    )
        (actualAndExpected level output expected)


test3 : () -> TestResult
test3 () =
    let
        moveActions =
            [ Just MoveRight, Just MoveRight ]

        level =
            Level.init
                { playerStart = ( 1, 1 )
                , walls = [ ( 0, 0 ), ( 0, 1 ), ( 4, 1 ) ] |> Set.fromList
                , boxesStart = [ ( 2, 1 ) ] |> Set.fromList
                , exit =
                    { position = ( 3, 5 )
                    , tileEdge = LeftEdge
                    }
                , levelSize = ( 5, 5 )
                , portalPairs =
                    [ { firstPortal = { position = ( 0, 2 ), tileEdge = LeftEdge }
                      , secondPortal = { position = ( 3, 1 ), tileEdge = RightEdge }
                      , timeDelta = 2
                      }
                    ]
                , doors = []
                }
                |> unwrapResult

        output =
            LevelState.timeline level moveActions

        expected =
            [ ( 0, { boxes = [ { position = ( 0, 2 ) }, { position = ( 2, 1 ) } ], players = [ { age = 0, position = ( 1, 1 ) } ] } )
            , ( 1, { boxes = [ { position = ( 0, 2 ) }, { position = ( 3, 1 ) } ], players = [ { age = 1, position = ( 2, 1 ) } ] } )
            , ( 2, { boxes = [ { position = ( 0, 2 ) } ], players = [ { age = 2, position = ( 3, 1 ) } ] } )
            ]
                |> RegularDict.fromList
    in
    (if output == expected then
        Passed

     else
        Failed
    )
        (actualAndExpected level output expected)


test1 : () -> TestResult
test1 () =
    let
        moveActions =
            [ Just MoveRight, Just MoveRight, Just MoveRight ]

        level =
            Level.init
                { playerStart = ( 3, 3 )
                , walls = [] |> Set.fromList
                , boxesStart = [] |> Set.fromList
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
                , doors = []
                }
                |> unwrapResult

        output =
            LevelState.timeline level moveActions

        expected =
            [ ( 0, { boxes = [], players = [ { age = 2, position = ( 0, 2 ) }, { age = 0, position = ( 3, 3 ) } ] } )
            , ( 1, { boxes = [], players = [ { age = 3, position = ( 1, 2 ) }, { age = 1, position = ( 4, 3 ) } ] } )
            ]
                |> RegularDict.fromList
    in
    (if output == expected then
        Passed

     else
        Failed
    )
        (actualAndExpected level output expected)


actualAndExpected level actual expected =
    Element.column
        [ Element.spacing 4 ]
        [ Element.el [ Element.Font.size 14 ] (Element.text "Actual:")
        , showInstants level actual
        , Element.el [ Element.Font.size 14 ] (Element.text "Expected:")
        , showInstants level expected
        ]


test2 : () -> TestResult
test2 () =
    let
        moveActions =
            [ Just MoveLeft, Just MoveLeft, Just MoveLeft ]

        level =
            Level.init
                { playerStart = ( 1, 2 )
                , walls = [] |> Set.fromList
                , boxesStart = [] |> Set.fromList
                , exit =
                    { position = ( 3, 1 )
                    , tileEdge = LeftEdge
                    }
                , levelSize = ( 5, 5 )
                , portalPairs =
                    [ { firstPortal = { position = ( 0, 2 ), tileEdge = LeftEdge }
                      , secondPortal = { position = ( 4, 3 ), tileEdge = RightEdge }
                      , timeDelta = 1
                      }
                    ]
                , doors = []
                }
                |> unwrapResult

        output =
            LevelState.timeline level moveActions

        expected =
            [ ( 0, { boxes = [], players = [ { age = 0, position = ( 1, 2 ) } ] } )
            , ( 1, { boxes = [], players = [ { age = 1, position = ( 0, 2 ) } ] } )
            , ( 2, { boxes = [], players = [] } )
            , ( 3, { boxes = [], players = [ { age = 2, position = ( 4, 3 ) } ] } )
            , ( 4, { boxes = [], players = [ { age = 3, position = ( 3, 3 ) } ] } )
            ]
                |> RegularDict.fromList
    in
    (if output == expected then
        Passed

     else
        Failed
    )
        (actualAndExpected level output expected)


test4 : () -> TestResult
test4 () =
    let
        moveActions =
            [ Just MoveRight, Just MoveRight, Just MoveLeft ]

        level =
            Level.init
                { playerStart = ( 2, 1 )
                , walls = [] |> Set.fromList
                , boxesStart = [] |> Set.fromList
                , exit =
                    { position = ( 3, 5 )
                    , tileEdge = LeftEdge
                    }
                , levelSize = ( 5, 5 )
                , portalPairs =
                    [ { firstPortal = { position = ( 0, 2 ), tileEdge = LeftEdge }
                      , secondPortal = { position = ( 3, 1 ), tileEdge = RightEdge }
                      , timeDelta = 1
                      }
                    ]
                , doors = []
                }
                |> unwrapResult

        output =
            LevelState.timeline level moveActions

        expected =
            [ ( 0, { boxes = [], players = [ { age = 0, position = ( 2, 1 ) } ] } )
            , ( 1, { boxes = [], players = [ { age = 2, position = ( 0, 2 ) }, { age = 1, position = ( 3, 1 ) } ] } )
            , ( 2, { boxes = [], players = [] } )
            , ( 3, { boxes = [], players = [ { age = 3, position = ( 3, 1 ) } ] } )
            ]
                |> RegularDict.fromList
    in
    (if output == expected then
        Passed

     else
        Failed
    )
        (actualAndExpected level output expected)


showInstants : Level -> RegularDict.Dict Int LevelInstant -> Element msg
showInstants level dict =
    RegularDict.keys dict
        |> List.sort
        |> List.map (Frontend.viewLevel level dict)
        |> Element.row [ Element.spacing 8 ]


unwrapResult : Result e a -> a
unwrapResult result =
    case result of
        Ok ok ->
            ok

        Err _ ->
            Debug.todo ""
