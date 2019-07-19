module Tests exposing (all)

import Expect
import Test exposing (..)



-- Check out http://package.elm-lang.org/packages/elm-community/elm-test/latest to learn more about testing in Elm!


all : Test
all =
    describe "A Test Suite"
        [ test "Basic movement" <|
            \_ ->
                let
                    startState =
                        """┌─┐┌─┐┌─┐┌─┐┌─┐
                           │█││ ││ ││ ││ │
                           └─┘└─┘└─┘└─┘└─┘
                           ┌─┐┌─┐┌─┐┌─┐┌─┐
                           │█││P││ ││E││ │
                           └─┘└─┘└─┘└─┘└─┘
                           ┌─┐┌─┐┌─┐┌─┐┌─┐
                           │ ││ ││ ││ ││ │
                           └─┘└─┘└─┘└─┘└─┘
                           ┌─┐┌─┐┌─┐┌─┐┌─┐
                           │ ││ │A ││ ││ a
                           └─┘└─┘└─┘└─┘└─┘
                           ┌─┐┌─┐┌─┐┌─┐┌─┐
                           │ ││ ││ ││ ││ │
                           └─┘└─┘└─┘└─┘└─┘
                           
                           Time: 0
                           
                           A -> a: 2"""
                in
                Debug.todo ""
        ]
