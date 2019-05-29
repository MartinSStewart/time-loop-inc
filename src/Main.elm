module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (src)
import Level exposing (Level)
import LevelState exposing (LevelState, PlayerAction)
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
    , walls = Set.empty
    , boxesStart = [ ( 2, 1 ) ] |> Set.fromList
    , exit = ( 3, 1 )
    , levelSize = ( 5, 5 )
    , portalPairs = []
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


view : Model -> Html Msg
view model =
    div []
        [ img [ src "/logo.svg" ] []
        , h1 [] [ text "Your Elm App is working!" ]
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
