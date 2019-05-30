module Main exposing (Model, Msg(..), init, main, update, view)

import Array exposing (Array)
import Array2D exposing (Array2D)
import AsciiRender
import Basics.Extra exposing (flip)
import Browser
import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (src)
import Level exposing (Level, TileEdge(..))
import LevelState exposing (LevelState, PlayerAction, TimeState)
import Point exposing (Point)
import Set exposing (Set)



---- MODEL ----


type alias Model =
    { playerActions : List PlayerAction
    , currentTime : Int
    , levelState : LevelState
    }


getLevel : Level
getLevel =
    { playerStart = ( 1, 1 )
    , walls = [ ( 0, 0 ), ( 0, 1 ) ] |> Set.fromList
    , boxesStart = [ ( 2, 1 ) ] |> Set.fromList
    , exit = ( 3, 1 )
    , levelSize = ( 5, 5 )
    , portalPairs =
        [ { firstPortal = { position = ( 2, 3 ), tileEdge = LeftEdge }
          , secondPortal = { position = ( 4, 3 ), tileEdge = RightEdge }
          , timeDelta = 2
          }
        , { firstPortal = { position = ( 0, 4 ), tileEdge = TopEdge }
          , secondPortal = { position = ( 4, 4 ), tileEdge = BottomEdge }
          , timeDelta = 2
          }
        ]
    }


init : ( Model, Cmd Msg )
init =
    ( { playerActions = []
      , currentTime = 0
      , levelState = LevelState.init getLevel
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


tempTimeState =
    { playerPrime = ( 1, 1 )
    , players = [ ( 1, 2 ) ]
    , boxes = [ ( 2, 2 ) ]
    }


view : Model -> Html Msg
view model =
    div []
        [ h1
            [ Html.Attributes.style "white-space" "pre-line"
            , Html.Attributes.style "line-height" "40px"
            , Html.Attributes.style "letter-spacing" "10px"
            , Html.Attributes.style "font-size" "50px"
            , Html.Attributes.style "font-family" "courier"
            ]
            [ text <| AsciiRender.view getLevel tempTimeState 0 ]
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
