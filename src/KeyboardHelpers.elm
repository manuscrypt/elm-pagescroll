module KeyboardHelpers exposing (directionToTuple)

import Keyboard.Extra as Keyboard exposing (Direction)


directionToTuple : Keyboard.Direction -> ( Int, Int )
directionToTuple dir =
    case dir of
        Keyboard.North ->
            ( 0, 1 )

        Keyboard.NorthEast ->
            ( 1, 1 )

        Keyboard.East ->
            ( 1, 0 )

        Keyboard.SouthEast ->
            ( 1, -1 )

        Keyboard.South ->
            ( 0, -1 )

        Keyboard.SouthWest ->
            ( -1, -1 )

        Keyboard.West ->
            ( -1, 0 )

        Keyboard.NorthWest ->
            ( -1, 1 )

        Keyboard.NoDirection ->
            ( 0, 0 )
