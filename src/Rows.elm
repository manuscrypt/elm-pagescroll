module Rows exposing (..)

import VirtualDom


type alias Cell a = VirtualDom.Node a


type Cols a
    = Cols (List (Cell a)) (Cell a) (List (Cell a))


type Rows a
    = Rows (List (Cols a)) (Cols a) (List (Cols a))


type alias Dir =
    ( Float, Float )



--SHIFT 


shift : Rows a -> Dir -> Rows a
shift (Rows top mid bot) dir =
    case dir of
        ( -1, 0 ) ->
            Rows top (shiftLeft mid) bot

        ( 1, 0 ) ->
            Rows top (shiftRight mid) bot

        ( 0, 1 ) ->
            shiftUp (Rows top mid bot)

        ( 0, -1 ) ->
            shiftDown (Rows top mid bot)

        ( _, _ ) ->
            (Rows top mid bot)


shiftUp : Rows a -> Rows a
shiftUp (Rows top mid bot) =
    case top of
        [] ->
            Rows top mid bot

        newMid :: newTop ->
            Rows newTop newMid (mid :: bot)


shiftDown : Rows a -> Rows a
shiftDown (Rows top mid bot) =
    case bot of
        [] ->
            Rows top mid bot

        newMid :: newBot ->
            Rows (mid :: top) newMid newBot


shiftLeft : Cols a -> Cols a
shiftLeft (Cols left center right) =
    case left of
        [] ->
            Cols left center right

        newCenter :: newLeft ->
            Cols newLeft newCenter (center :: right)


shiftRight : Cols a -> Cols a
shiftRight (Cols left center right) =
    case right of
        [] ->
            Cols left center right

        newCenter :: newRight ->
            Cols (center :: left) newCenter newRight
