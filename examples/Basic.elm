module Basic exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Attributes as HA exposing (..)
import Svg
import Svg.Attributes as SA
import PageScroll exposing (..)
import AnimationFrame
import Animation exposing (Animation)


type alias Model =
    { anim : Animation Float
    , pages : PageScroll.Model
    }


type Msg
    = Animate Float
    | ScrollMsg PageScroll.Msg


main : Program Never
main =
    App.program
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        animSub =
            if Animation.isDone model.anim then
                Sub.none
            else
                AnimationFrame.diffs Animate
    in
        Sub.batch [ Sub.map ScrollMsg PageScroll.subscriptions, animSub ]


init : ( Model, Cmd Msg )
init =
    let
        ( p, px ) =
            PageScroll.init pages
    in
        { anim = Animation.immediately 0, pages = p } ! [ Cmd.map ScrollMsg px ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ScrollMsg psMsg ->
            let
                ( p, px ) =
                    PageScroll.update psMsg model.pages
            in
                { model | pages = p } ! [ Cmd.map ScrollMsg px ]

        Animate dt ->
            { model | anim = Animation.run dt model.anim } ! []


view : Model -> Html Msg
view model =
    App.map ScrollMsg <| PageScroll.view model.pages

pages : Rows
pages =
    Rows [ Cols [] (samplePage 1 "1" "#875692") [] ]
        (Cols [ samplePage 2 "2.1" "#F38400" ] (samplePage 3 "2.2" "#A1CAF1") [ samplePage 4 "2.3" "#BE0032" ])
        [ Cols [] (samplePage 5 "3" "#C2B280") [] ]


samplePage : Int -> String -> String -> Cell
samplePage idx txt hex =
    let
        svg =
            Svg.svg
                [ SA.width "100%"
                , SA.height "100%"
                , SA.viewBox "-15 -15 30 30"
                ]
                [ Svg.circle
                    [ SA.cx "0"
                    , SA.cy "0"
                    , SA.r "6"
                    , SA.style "fill:green; stroke: yellow; stroke-width: 0.25"
                    ]
                    []
                , Svg.text'
                    [ SA.cx "0"
                    , SA.cy "0"
                    , SA.fontFamily "Courier"
                    , SA.fontSize "5"
                    , SA.fill "black"
                    , SA.textAnchor "middle"
                    , SA.alignmentBaseline "middle"
                    ]
                    [ Svg.text txt ]
                ]

        --
    in
        Html.div
            [ HA.style
                [ (,) "background-color" hex
                , (,) "font-family" "monospace"
                , (,) "font-size" "64px"
                , (,) "font-weight" "bold"
                , (,) "text-align" "center"
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
                [ svg ]
            ]
