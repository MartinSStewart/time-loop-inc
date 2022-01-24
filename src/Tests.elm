module Tests exposing (..)

import AssocSet as Set
import Dict as RegularDict
import Element exposing (Element)
import Element.Font
import Frontend
import Html exposing (Html)
import Level exposing (Level, TileEdge(..))
import LevelState exposing (MoveAction(..))


main : Html ()
main =
    List.map
        viewTestResult
        [ test "Can push box" test0
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


test : String -> TestResult -> Test
test name testResult =
    Test name testResult


type TestResult
    = Passed (Element ())
    | Failed (Element ())


type Test
    = Test String TestResult


test0 : TestResult
test0 =
    let
        moveActions =
            [ Just MoveRight ]

        output =
            LevelState.timeline level0 moveActions

        expected =
            [ ( 2, { boxes = [ { position = ( 3, 1 ) } ], players = [ { age = 2, position = ( 2, 1 ) } ] } )
            , ( 1, { boxes = [ { position = ( 3, 1 ) } ], players = [ { age = 1, position = ( 2, 1 ) } ] } )
            , ( 0, { boxes = [ { position = ( 2, 1 ) } ], players = [ { age = 0, position = ( 1, 1 ) } ] } )
            ]
                |> RegularDict.fromList
    in
    (if output == expected then
        Passed

     else
        Failed
    )
        (Element.row
            [ Element.spacing 8 ]
            [ Frontend.viewLevel level0 output 0
            , Frontend.viewLevel level0 output 1
            ]
        )


test1 : TestResult
test1 =
    let
        moveActions =
            [ Just MoveRight, Just MoveRight, Just MoveRight ]

        output =
            LevelState.timeline level1 moveActions

        expected =
            [ ( -1, { boxes = [], players = [ { age = 2, position = ( 0, 2 ) }, { age = -1, position = ( 3, 3 ) } ] } )
            , ( 0, { boxes = [], players = [ { age = 3, position = ( 1, 2 ) }, { age = 0, position = ( 3, 3 ) } ] } )
            , ( 1, { boxes = [], players = [ { age = 4, position = ( 1, 2 ) }, { age = 1, position = ( 4, 3 ) } ] } )
            , ( 2, { boxes = [], players = [ { age = 5, position = ( 1, 2 ) } ] } )
            , ( 3, { boxes = [], players = [ { age = 6, position = ( 1, 2 ) } ] } )
            , ( 4, { boxes = [], players = [ { age = 7, position = ( 1, 2 ) } ] } )
            ]
                |> RegularDict.fromList
    in
    (if output == expected then
        Passed

     else
        Failed
    )
        (Element.column
            [ Element.spacing 4 ]
            [ Element.el [ Element.Font.size 14 ] (Element.text "Actual:")
            , Element.row
                [ Element.spacing 8 ]
                [ Frontend.viewLevel level1 output -1
                , Frontend.viewLevel level1 output 0
                , Frontend.viewLevel level1 output 1
                ]
            , Element.el [ Element.Font.size 14 ] (Element.text "Expected:")
            , Element.row
                [ Element.spacing 8 ]
                [ Frontend.viewLevel level1 expected -1
                , Frontend.viewLevel level1 expected 0
                , Frontend.viewLevel level1 expected 1
                ]
            ]
        )


test2 : TestResult
test2 =
    let
        moveActions =
            [ Just MoveLeft, Just MoveLeft, Just MoveLeft ]

        output =
            LevelState.timeline level2 moveActions

        expected =
            [ ( 0, { boxes = [], players = [ { age = 0, position = ( 1, 2 ) } ] } )
            , ( 1, { boxes = [], players = [ { age = 1, position = ( 0, 2 ) } ] } )
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
        (Element.column
            [ Element.spacing 4 ]
            [ Element.el [ Element.Font.size 14 ] (Element.text "Actual:")
            , Element.row
                [ Element.spacing 8 ]
                [ Frontend.viewLevel level2 output 0
                , Frontend.viewLevel level2 output 1
                , Frontend.viewLevel level2 output 2
                , Frontend.viewLevel level2 output 3
                , Frontend.viewLevel level2 output 4
                ]
            , Element.el [ Element.Font.size 14 ] (Element.text "Expected:")
            , Element.row
                [ Element.spacing 8 ]
                [ Frontend.viewLevel level2 expected 0
                , Frontend.viewLevel level2 expected 1
                , Frontend.viewLevel level2 expected 2
                , Frontend.viewLevel level2 expected 3
                , Frontend.viewLevel level2 expected 4
                ]
            ]
        )


level0 : Level
level0 =
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
        }
        |> unwrapResult


level1 : Level
level1 =
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
        }
        |> unwrapResult


level2 : Level
level2 =
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
              , timeDelta = 2
              }
            ]
        }
        |> unwrapResult


unwrapResult result =
    case result of
        Ok ok ->
            ok

        Err _ ->
            Debug.todo ""
