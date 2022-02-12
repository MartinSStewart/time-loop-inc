module KeyHelper exposing (KeyState, down, pressed, redo, undo)

import Keyboard exposing (Key)


type alias KeyState a =
    { a | keys : List Key, previousKeys : List Key }


down : Keyboard.Key -> KeyState a -> Bool
down key keyState =
    List.any ((==) key) keyState.keys


pressed : Keyboard.Key -> KeyState a -> Bool
pressed key keyState =
    down key keyState && (List.any ((==) key) keyState.previousKeys |> not)


undo : KeyState a -> Bool
undo keyState =
    down Keyboard.Control keyState && pressed (Keyboard.Character "Z") keyState


redo : KeyState a -> Bool
redo keyState =
    down Keyboard.Control keyState && pressed (Keyboard.Character "Y") keyState
