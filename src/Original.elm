module Original exposing (..)

import Html exposing (..)
import Html.Attributes as HA exposing (..)
import Rows exposing (..)
import Svg
import Svg.Attributes as SA
import Color.Convert


-- import Util
--import Formatting exposing (..)

import KellyColors


t : Int -> String -> Cell a
t i str =
    let
        kc =
            KellyColors.at (i + 3)
    in
        Html.div
            [ HA.style
                [ (,) "background-color" <| Color.Convert.colorToHex kc
                , (,) "font-family" "monospace"
                , (,) "font-size" "64px"
                , (,) "font-weight" "bold"
                , (,) "width" "100%"
                , (,) "height" "100%"
                , (,) "display" "table"
                ]
            ]
            [ Html.div
                [ HA.style
                    [ (,) "vertical-align" "middle"
                    , (,) "display" "table-cell"
                    , (,) "text-align" "center"
                    ]
                ]
                [ Html.text str ]
            , Svg.circle [ SA.cx "10", SA.cy "20", SA.r "50", SA.style "fill:red" ] []
            ]


pages : Rows a
pages =
    Rows.Rows [ Rows.Cols [] (t 1 "1") [] ] (Rows.Cols [ t 2 "2.1" ] (t 3 "2.2") [ t 4 "2.3" ]) [ Rows.Cols [] (t 5 "3") [] ]
