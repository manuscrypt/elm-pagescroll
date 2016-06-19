module Pane exposing (..)

import Svg exposing (Svg)
import Html exposing (Html)
import Svg.Attributes as SA
import Html.App as App
import VirtualDom
import Animation exposing (Animation)
import AnimationFrame
import Time exposing (Time)
import Window
import String
import Task.Extra as Task
import Keyboard.Extra as Keyboard exposing (Direction)
import Ease exposing (outQuint, inOutBack)
import KeyboardHelpers exposing (directionToTuple)


type alias Cell a =
    VirtualDom.Node a


type Cols a
    = Cols (List (Cell a)) (Cell a) (List (Cell a))


type Rows a
    = Rows (List (Cols a)) (Cols a) (List (Cols a))



-- animation : Animation (Window.Size -> Svg Msg)
-- zeroState : Window.Size -> Svg Msg
-- zeroState =
--     (\s -> Svg.circle [ SA.cx "0", SA.cy "0", SA.r <| toString s.width ] [])


type alias Model a =
    { animation : Animation Float
    , rows : Rows a
    , windowSize : Window.Size
    , keyboardModel : Keyboard.Model
    , curDir : ( Int, Int )
    }


type Msg
    = MouseOver
    | NoOp
    | Animate Time
    | OnSizeChanged Window.Size
    | KeyboardMsg Keyboard.Msg
    | Scroll ( Int, Int )


fromText : String -> Cell a
fromText str =
    Html.div [] [ Html.text str ]


content : Rows a
content =
    let
        topRow =
            Cols [] (fromText "top") []

        centerRow =
            Cols [] (fromText "This") [ fromText "is", fromText "just", fromText "a", fromText "test" ]

        botRow1 =
            Cols [ fromText "bl1", fromText "bl2" ] (fromText "bot") [ fromText "br1" ]

        botRow2 =
            Cols [] (fromText "bot") [ fromText "br21", fromText "br22" ]
    in
        Rows [ topRow ] centerRow [ botRow1, botRow2 ]


main : Program Never
main =
    App.program
        { init = init { width = 0, height = 0 }
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


sizeToMsg : Window.Size -> Msg
sizeToMsg size =
    OnSizeChanged size


subscriptions : { b | animation : Animation a } -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map KeyboardMsg Keyboard.subscriptions
        , Window.resizes sizeToMsg
        , if Animation.isDone model.animation then
            Sub.none
          else
            AnimationFrame.diffs Animate
        ]


init : Window.Size -> ( Model a, Cmd Msg )
init size =
    let
        ( keyboardModel, keyboardCmd ) =
            Keyboard.init
    in
        { windowSize = size
        , rows = content
        , animation = halfPulse
        , keyboardModel = keyboardModel
        , curDir = ( 0, 0 )
        }
            ! [ Cmd.map KeyboardMsg keyboardCmd, initWindowSize ]


halfPulse : Animation Float
halfPulse =
    (1 * Time.second)
        |> Animation.interval
        |> Animation.map Ease.inOutBack


initWindowSize : Cmd Msg
initWindowSize =
    Window.size |> Task.performFailproof sizeToMsg


update : Msg -> Model a -> ( Model a, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
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
            if (fst dir == 0) then
                model ! []
            else if (dir == model.curDir) then
                model ! []
            else
                { model | animation = Animation.reverse model.animation, curDir = dir } ! []

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
                        { model | animation = animated, rows = shiftCols model, curDir = ( 0, 0 ) } ! []


shiftCols : Model a -> Rows a
shiftCols model =
    let
        (Rows top mid bot) =
            model.rows
    in
        if (fst model.curDir == -1) then
            Debug.log "shift left" <| Rows top (shiftLeft mid) bot
        else
            Debug.log "shift right" <| Rows top (shiftRight mid) bot


shiftLeft : Cols a -> Cols a
shiftLeft (Cols left center right) =
    if (List.length right == 0) then
        Cols left center right
    else
        let
            newLeft =
                left ++ [ center ]

            newCenter =
                Maybe.withDefault (Svg.g [] []) <| List.head right

            newRight =
                List.drop 1 right
        in
            Debug.log "newLeft" <| Cols newLeft newCenter newRight


shiftRight : Cols a -> Cols a
shiftRight (Cols left center right) =
    if (List.length left == 0) then
        (Cols left center right)
    else
        let
            newLeft =
                List.take (-1 + List.length left) left

            newCenter =
                Maybe.withDefault (Svg.g [] []) <| List.head <| List.reverse left

            newRight =
                [ center ] ++ right
        in
            Debug.log "newRight" <| Cols newLeft newCenter newRight


toSvg : Float -> Window.Size -> Svg Msg
toSvg t { width, height } =
    Svg.circle
        [ SA.r <| toString <| 0.3 * t * (toFloat (min width height))
        , SA.cx "0"
        , SA.cy "0"
        , SA.stroke "#000000"
        , SA.fillOpacity "0"
        , SA.strokeWidth "5"
        ]
        []


windowSize : Model a -> Window.Size
windowSize m =
    { width = round (0.9 * (toFloat m.windowSize.width)), height = round (0.9 * (toFloat m.windowSize.height)) }


cellSize : Model a -> Window.Size
cellSize m =
    let
        { width, height } =
            m.windowSize

        max =
            min width height
    in
        { width = max // 4, height = max // 4 }


centerCircle : Svg a
centerCircle =
    Svg.circle [ SA.cx "0", SA.cy "0", SA.r "3", SA.fill "green" ] []


view : Model a -> Svg Msg
view m =
    let
        { width, height } =
            windowSize m
    in
        Svg.svg
            [ SA.width <| toString width
            , SA.height <| toString height
            , SA.viewBox <| viewBox <| windowSize m
            , SA.style "border: 1px solid #cccccc;"
            ]
            [ viewRows m
            , centerCircle
            ]


viewRows : Model a -> Svg Msg
viewRows model =
    let
        (Rows top cur bot) =
            model.rows

        rows =
            top ++ [ cur ] ++ bot

        mapRow =
            viewCols model { count = List.length rows }
    in
        Svg.g [] (List.indexedMap mapRow rows)


viewCols : Model a -> { count : Int } -> Int -> Cols a -> Svg Msg
viewCols model colCount idx (Cols left mid right) =
    let
        cols =
            left ++ [ mid ] ++ right

        mapCol =
            viewCell model { count = List.length cols }
    in
        Svg.g [] (List.indexedMap mapCol cols)


viewCell : Model a -> { count : Int } -> Int -> Cell a -> Svg Msg
viewCell model { count } idx cell =
    let
        size =
            cellSize model

        offAnim =
            Animation.sample model.animation |> (*) (toFloat size.width) |> round

        offPage =
            (-count // 2 + idx) * size.width
    in
        App.map (\_ -> NoOp)
            <| Svg.g [ offset ( offPage + offAnim, 0 ) ]
                [ Svg.rect (defaultRect size.width "stroke:red;line-style:dashed;fill:none") []
                , embedHtml size.width cell
                ]


embedHtml : Int -> Cell a -> Svg a
embedHtml size model =
    Svg.foreignObject (defaultRect 50 "fill:red") [ Html.body [] [ model ] ]


offset : ( number, number ) -> Svg.Attribute b
offset ( x, y ) =
    SA.transform <| "translate (" ++ (toString x) ++ "," ++ (toString y) ++ ")"


defaultRect : Int -> String -> List (Svg.Attribute a)
defaultRect size style =
    [ SA.x <| toString (-size // 2)
    , SA.y <| toString (-size // 2)
    , SA.width <| toString size
    , SA.height <| toString size
    , SA.fill "none"
    , SA.style style
    ]


viewBox : { a | height : Int, width : Int } -> String
viewBox { width, height } =
    String.join " " <| List.map toString [ -width // 2, -height // 2, width, height ]



-- viewRows : Window.Size->{ rowCount:Int }->Int ->Rows a -> Svg a
-- viewRows size rowCount idx (Rows top mid bot) =
--     let rows = top ++ [mid] ++ bot
--     in Svg.g [] (List.indexedMap (viewCols size { colCount = List.length rows }) rows)
-- viewCell : String  -> Int -> Int -> Html a -> Svg Msg
-- viewCell style size idx cell =
--     Svg.g [ offsetBy (30 * idx), SA.style style ] [ App.map (\s -> NoOp) <| embedHtml size cell ]
-- offsetBy : a -> Svg.Attribute b
-- offsetBy offset =
--     SA.transform <| "translate (" ++ (toString offset) ++ ",0)"
