module Util exposing (..)

import Math.Vector2 exposing (Vec2, vec2, getX, getY, fromTuple)
import Array exposing (get, set, Array)
import String

import Svg exposing (Svg)
import Svg.Attributes as SA


noFx : a -> ( a, Cmd b )
noFx m =
    ( m, Cmd.none )


vecToSvgPos : Vec2 -> String
vecToSvgPos vec =
    (toString <| getX vec) ++ " " ++ (toString <| getY vec)


warningsFor : String -> String
warningsFor moduleFileName =
    "elm-make --warn " ++ moduleFileName


getAt : Int -> Array number -> number
getAt idx arr =
    Array.get idx arr |> Maybe.withDefault 0


setAt : Int -> number -> Array number -> Array number
setAt idx val arr =
    Array.set idx val arr


updateMany : List a -> (a -> b -> ( b, Cmd a )) -> ( b, Cmd a ) -> ( b, Cmd a )
updateMany msgs update modelCmd =
    List.foldl (updateOne update) modelCmd msgs


updateOne : (a -> b -> ( b, Cmd a )) -> a -> ( b, Cmd a ) -> ( b, Cmd a )
updateOne update msg ( model, effect ) =
    let
        ( next, nextEffect ) =
            update msg model
    in
        next ! [ effect, nextEffect ]


fromIntRecord : { a | x : Int, y : Int } -> Vec2
fromIntRecord { x, y } =
    fromTuple ( toFloat x, toFloat y )


fromIntTuple : ( Int, Int ) -> Vec2
fromIntTuple ( x, y ) =
    fromIntRecord { x = x, y = y }


toIntRecord : Vec2 -> { x : Int, y : Int }
toIntRecord v =
    { x = round <| getX v, y = round <| getY v }


toIntTuple : Vec2 -> ( Int, Int )
toIntTuple v =
    ( round <| getX v, round <| getY v )


multiplyVec : Vec2 -> Vec2 -> Vec2
multiplyVec v1 v2 =
    vec2 (getX v1 * getX v2) (getY v1 * getY v2)


    