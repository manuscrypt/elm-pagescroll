module Basic exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Attributes as HA exposing (..)
import Svg
import Svg.Attributes as SA
import PageScroll exposing (..)


--init, update, view, subscriptions, Cell, Rows, Cols)


main : Program Never
main =
    App.program
        { init = init pages
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


samplePage : Int -> String -> String -> Cell a
samplePage idx txt hex =
    Html.div
        [ HA.style
            [ (,) "background-color" hex
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
            --[]
            [  Svg.text txt, Svg.svg   [ SA.width "60"
                   , SA.height "60"
                   , SA.viewBox "-20 -20 40 40"
                   ] [ Svg.circle [ SA.cx "0", SA.cy "0", SA.r "10", SA.style "fill:red; stroke: black" ] []
                     ]
                     
            ]
        ]


pages : Rows a
pages =
    Rows [ Cols [] (samplePage 1 "1" "#875692") [] ]
        (Cols [ samplePage 2 "2.1" "#F38400" ] (samplePage 3 "2.2" "#A1CAF1") [ samplePage 4 "2.3" "#BE0032" ])
        [ Cols [] (samplePage 5 "3" "#C2B280") [] ]
