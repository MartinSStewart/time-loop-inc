module Main exposing (Model, Msg(..), init, main, update, view)

import Array exposing (Array)
import Array2D exposing (Array2D)
import AsciiRender
import Basics.Extra exposing (flip)
import Browser
import Browser.Events
import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (src)
import Keyboard exposing (Key)
import Level exposing (Level, TileEdge(..))
import LevelState exposing (LevelState, MoveAction(..))
import List.Extra as List
import Maybe.Extra as Maybe
import Point exposing (Point)
import Set exposing (Set)
import Time exposing (Posix)



---- MODEL ----


type alias Model =
    { playerActions : List MoveAction
    , currentTime : Int
    , keys : List Key
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
        ]
    }


init : ( Model, Cmd Msg )
init =
    ( { playerActions = []
      , currentTime = 0
      , keys = []
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = NoOp
    | KeyMsg Keyboard.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model |> addCmdNone

        KeyMsg keyMsg ->
            let
                newKeys =
                    Keyboard.update keyMsg model.keys

                keyDown key =
                    List.any ((==) key) newKeys

                keyPressed key =
                    keyDown key && (List.any ((==) key) model.keys |> not)

                model_ =
                    case ( keyDown Keyboard.Control, keyPressed (Keyboard.Character "Z") ) of
                        ( True, True ) ->
                            { model
                                | playerActions =
                                    model.playerActions |> List.reverse |> List.drop 1 |> List.reverse
                            }

                        _ ->
                            model

                action =
                    if keyPressed Keyboard.ArrowLeft then
                        Just MoveLeft

                    else if keyPressed Keyboard.ArrowRight then
                        Just MoveRight

                    else if keyPressed Keyboard.ArrowUp then
                        Just MoveUp

                    else if keyPressed Keyboard.ArrowDown then
                        Just MoveDown

                    else if keyPressed Keyboard.Spacebar then
                        Just MoveNone

                    else
                        Nothing
            in
            { model_ | keys = newKeys, playerActions = model_.playerActions ++ Maybe.toList action }
                |> addCmdNone


addCmdNone : a -> ( a, Cmd msg )
addCmdNone a =
    ( a, Cmd.none )


addCmd : Cmd msg -> a -> ( a, Cmd msg )
addCmd cmd a =
    ( a, cmd )



---- VIEW ----


tempTimeState =
    { playerPrime = ( ( 1, 1 ), [] )
    , players = []
    , boxes = []
    }


view : Model -> Html Msg
view model =
    div []
        [ h1
            [ Html.Attributes.style "white-space" "pre-line"
            , Html.Attributes.style "line-height" "24px"
            , Html.Attributes.style "letter-spacing" "10px"
            , Html.Attributes.style "font-size" "30px"
            , Html.Attributes.style "font-family" "courier"
            ]
            [ text <| AsciiRender.view getLevel tempTimeState 0 ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map KeyMsg Keyboard.subscriptions
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }
