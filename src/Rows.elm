module Rows exposing (..)

import VirtualDom

type alias Cell a =
    VirtualDom.Node a


type Cols a
    = Cols (List (Cell a)) (Cell a) (List (Cell a))


type Rows a
    = Rows (List (Cols a)) (Cols a) (List (Cols a))


type alias Dir =
    ( Int, Int )


maxCols: Rows a -> Int
maxCols (Rows top (Cols l c r) bot) =
    let lensTop = Maybe.withDefault 0 <| List.maximum <| List.map (\(Cols left center right) -> List.length left + List.length right +1 ) top
        lensBot = Maybe.withDefault 0 <| List.maximum <| List.map (\(Cols left center right) -> List.length left + List.length right +1 ) bot
        lensMid = List.length l + List.length r + 1 
        m1 = Basics.max lensTop lensBot 
        m2 = Basics.max m1 lensMid
    in m2
     

canShift : Rows a -> Dir -> Bool
canShift  (Rows top (Cols left center right) bot) dir =
        case dir of
            ( 0, -1 ) ->
                (List.length top) > 0

            ( 0, 1 ) ->
                (List.length bot) > 0

            ( -1, 0 ) ->
                (List.length left) > 0

            ( 1, 0 ) ->
                (List.length right) > 0

            ( _, _ ) ->
                False



--SHIFT


shift : Rows a -> Dir -> Rows a
shift (Rows top mid bot) dir =
    case dir of
        ( -1, 0 ) ->
            Rows top (shiftLeft mid) bot

        ( 1, 0 ) ->
            Rows top (shiftRight mid) bot

        ( 0, 1 ) ->
            shiftDown (Rows top mid bot)

        ( 0, -1 ) ->
            shiftUp (Rows top mid bot)

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
            Rows (top ++ [mid]) newMid newBot


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
            Cols (left ++ [ center ]) newCenter newRight


