module Main exposing (..)

import Svg exposing (Svg)
import VirtualDom exposing (on, Property)
import Html exposing (Html)
import Html.Attributes as HA
import Html.App as App
import Svg.Attributes as SA
import Animation exposing (Animation)
import AnimationFrame
import Time exposing (Time)
import Window
import Task.Extra as Task
import Keyboard.Extra as Keyboard exposing (Direction)
import Ease exposing (outQuint, inOutBack)
import KeyboardHelpers exposing (directionToTuple)
import Util exposing (..)
import Rows exposing (Dir, Rows, Cols, Cell)
import Original
import Json.Decode as Json exposing ((:=))


type alias Model a =
    { animation : Animation ({ width : Float, height : Float } -> Dir -> ( Float, Float ))
    , rows : Rows a
    , scale : Float
    , windowSize : Window.Size
    , keyboardModel : Keyboard.Model
    , curDir : Dir
    }


type Msg
    = MouseOver
    | NoOp
    | Animate Time
    | OnSizeChanged Window.Size
    | KeyboardMsg Keyboard.Msg
    | Scroll Dir
    | Wheel ( Float, Float )


main : Program Never
main =
    App.program
        { init = init Original.pages 1
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


subscriptions : Model a -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map KeyboardMsg Keyboard.subscriptions
        , Window.resizes (\size -> OnSizeChanged size)
        , if Animation.isDone model.animation then
            Sub.none
          else
            AnimationFrame.diffs Animate
        ]


init : Rows a -> Float -> ( Model a, Cmd Msg )
init content scale =
    let
        ( keyboardModel, keyboardCmd ) =
            Keyboard.init
    in
        { windowSize = { width = 0, height = 0 }
        , rows = content
        , scale = scale
        , animation = Animation.immediately zeroState
        , keyboardModel = keyboardModel
        , curDir = ( 0, 0 )
        }
            ! [ Cmd.map KeyboardMsg keyboardCmd, Window.size |> Task.performFailproof (\s -> OnSizeChanged s) ]


scrollAnimation : Animation ({ width : Float, height : Float } -> Dir -> ( Float, Float ))
scrollAnimation =
    (0.7 * Time.second)
        |> Animation.interval
        |> Animation.map Ease.outQuint
        |> Animation.map
            (\t size dir ->
                ( -t * (fst dir) * size.width
                , t * (snd dir) * size.height
                )
            )


update : Msg -> Model a -> ( Model a, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        Wheel ( xx, yy ) ->
            let
                x =
                    Debug.log "wheel" "s"
            in
                model ! []

        KeyboardMsg keyMsg ->
            let
                ( keyboardModel, keyboardCmd ) =
                    Keyboard.update keyMsg model.keyboardModel

                dir =
                    directionToTuple
                        <| Keyboard.arrowsDirection keyboardModel

                ( model', cmd ) =
                    update (Scroll dir) { model | keyboardModel = keyboardModel }
            in
                model' ! [ Cmd.map KeyboardMsg keyboardCmd, cmd ]

        Scroll dir ->
            if not <| Animation.isDone model.animation then
                model ! []
            else
                let
                    shiftedRows =
                        Rows.shift model.rows dir
                in
                    if shiftedRows == model.rows then
                        model ! []
                    else
                        { model
                            | animation = scrollAnimation
                            , curDir = dir
                        }
                            ! []

        MouseOver ->
            model ! []

        OnSizeChanged size ->
            { model | windowSize = size } ! []

        Animate dt ->
            let
                animated =
                    Animation.run dt model.animation
            in
                case Animation.isDone animated of
                    False ->
                        { model | animation = animated } ! []

                    True ->
                        { model
                            | animation = Animation.immediately zeroState
                            , rows = Rows.shift model.rows model.curDir
                            , curDir = ( 0, 0 )
                        }
                            ! []


zeroState : { width : Float, height : Float } -> Dir -> ( Float, Float )
zeroState s d =
    ( 0, 0 )


windowSize : Model a -> { width : Float, height : Float }
windowSize m =
    { width = m.scale * (toFloat m.windowSize.width), height = m.scale * (toFloat m.windowSize.height) }


cellSize : Model a -> { width : Float, height : Float }
cellSize m =
    let
        { width, height } =
            windowSize m
    in
        { width = width, height = height }


view : Model a -> Svg Msg
view m =
    let
        { width, height } =
            windowSize m

        (Rows.Rows top mid bot) =
            m.rows

        allRows =
            List.reverse top ++ [ mid ] ++ bot

        size =
            cellSize m
    in
        Html.body [ HA.style [ (,) "overflow" "hidden", (,) "overflow-x" "hidden", (,) "overflow-y" "hidden", (,) "-ms-overflow-style" "none" ] ]
            [ Svg.svg
                [ SA.width <| toString width
                , SA.height <| toString height
                , SA.viewBox <| viewBox <| windowSize m
                , on "wheel" wx
                ]
                [ Svg.g [] <| List.indexedMap (viewCol m (List.length top)) allRows
                , Svg.circle [ SA.cx "0", SA.cy "0", SA.r "3", SA.fill "green" ] []
                ]
            ]


wx : Json.Decoder Msg
wx =
    Json.map Wheel <| Json.tuple2 (,) Json.float Json.float


viewCol : Model a -> Int -> Int -> Cols a -> Svg Msg
viewCol model rowCount idx (Rows.Cols left center right) =
    let
        all =
            List.reverse left ++ [ center ] ++ right

        size =
            cellSize model

        off =
            ( 0, (toFloat (-rowCount + idx)) * size.height )
    in
        let
            cols =
                List.indexedMap (viewCell model (List.length left)) all
        in
            Svg.g [ offset off ] cols


viewCell : Model a -> Int -> Int -> Cell a -> Svg Msg
viewCell model colCount idx cell =
    let
        size =
            cellSize model |> Debug.log "size"

        animOff =
            Animation.sample model.animation size model.curDir

        off =
            ( (toFloat <| (-colCount + idx)) * size.width, 0 )
    in
        cellSvg size cell (add animOff off)


nsXHtml : String
nsXHtml =
    "http://www.w3.org/1999/xhtml/"


cellSvg : { width : Float, height : Float } -> Html a -> ( number, number ) -> Html Msg
cellSvg size cell off =
    App.map (\_ -> NoOp)
        <| Svg.g [ offset off ]
            [ Svg.foreignObject
                [ SA.x <| toString (-size.width / 2)
                , SA.y <| toString (-size.height / 2)
                , SA.width <| toString size.width
                , SA.height <| toString size.height
                  --, SA.requiredExtensions nsXHtml
                ]
                [ Html.div
                    [ HA.attribute "xmlns" nsXHtml
                    , HA.style
                        [ (,) "width" "100%"
                        , (,) "height" "100%"
                        ]
                    ]
                    [ cell ]
                ]
            ]


add : ( number, number ) -> ( number, number ) -> ( number, number )
add ( a, b ) ( x, y ) =
    ( a + x, b + y )
