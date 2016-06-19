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
import Task.Extra as Task
import Keyboard.Extra as Keyboard exposing (Direction)
import Ease exposing (outQuint, inOutBack)
import KeyboardHelpers exposing (directionToTuple)
import Util exposing (..)


type alias Cell a =
    VirtualDom.Node a


type alias Dir =
    ( Int, Int )


type alias Page =
    ( Int, Int )


type Cols a
    = Cols (List (Cell a)) (Cell a) (List (Cell a))


type Rows a
    = Rows (List (Cols a)) (Cols a) (List (Cols a))


type alias Model a =
    { animation : Animation (Window.Size -> Dir -> ( Float, Float ))
    , rows : Rows a
    , windowSize : Window.Size
    , keyboardModel : Keyboard.Model
    , curDir : Dir
    , curPage : Page
    }


type Msg
    = MouseOver
    | NoOp
    | Animate Time
    | OnSizeChanged Window.Size
    | KeyboardMsg Keyboard.Msg
    | Scroll Dir


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


init : Window.Size -> ( Model a, Cmd Msg )
init size =
    let
        ( keyboardModel, keyboardCmd ) =
            Keyboard.init
    in
        { windowSize = size
        , rows = content
        , animation = scrollAnimation
        , keyboardModel = keyboardModel
        , curDir = ( 0, 0 )
        , curPage = ( 0, 0 )
        }
            ! [ Cmd.map KeyboardMsg keyboardCmd, Window.size |> Task.performFailproof (\s -> OnSizeChanged s) ]


scrollAnimation : Animation (Window.Size -> Dir -> ( Float, Float ))
scrollAnimation =
    (1 * Time.second)
        |> Animation.interval
        |> Animation.map Ease.inOutBack
        |> Animation.map
            (\t size dir ->
                ( t * (toFloat <| fst dir) * (toFloat <| size.width)
                , t * (toFloat <| snd dir) * (toFloat <| size.height)
                )
            )


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
            if (dir == ( 0, 0 )) then
                model ! []
            else if not (Animation.isDone model.animation) then
                if (dir == oppositeOf model.curDir) then
                    { model | animation = Animation.reverse model.animation, curDir = dir } ! []
                else
                    model ! []
            else
                { model | animation = scrollAnimation, curDir = dir } ! []

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
                            | animation = animated
                            , rows = shiftCols model
                            , curPage = add model.curPage model.curDir
                            , curDir = ( 0, 0 )
                        }
                            ! []


windowSize : Model a -> Window.Size
windowSize m =
    { width = round (0.9 * (toFloat m.windowSize.width)), height = round (0.9 * (toFloat m.windowSize.height)) }


cellSize : Model a -> Window.Size
cellSize m =
    let
        { width, height } =
            windowSize m

        max =
            min width height
    in
        { width = max // 4, height = max // 4 }


view : Model a -> Svg Msg
view m =
    let
        { width, height } =
            windowSize m

        (Rows top cur bot) =
            m.rows

        allRows =
            top ++ [ cur ] ++ bot
    in
        Svg.svg
            [ SA.width <| toString width
            , SA.height <| toString height
            , SA.viewBox <| viewBox <| windowSize m
            , SA.style "border: 1px solid #cccccc;"
            ]
            [ Svg.g [ offset <| pageOffset m ] (List.indexedMap (viewCol m (List.length allRows)) allRows)
            , Svg.circle [ SA.cx "0", SA.cy "0", SA.r "3", SA.fill "green" ] []
            ]


viewCol : Model a -> Int -> Int -> Cols a -> Svg Msg
viewCol model count idx (Cols left center right) =
    let
        all =
            left ++ [ center ] ++ right

        size =
            cellSize model

        rowOffset =
            ( 0, toFloat <| ((-count + 1) // 2 + idx) * size.height )
    in
        Svg.g [ offset rowOffset ] <| List.indexedMap (viewCell model (List.length all)) all


viewCell : Model a -> Int -> Int -> Cell a -> Svg Msg
viewCell model count idx cell =
    let
        size =
            cellSize model

        colOff =
            ( toFloat <| (-count // 2 + idx) * size.width, 0 )

        animOff =
            Animation.sample model.animation (cellSize model) model.curDir
    in
        App.map (\_ -> NoOp)
            <| Svg.g [ offset <| add animOff colOff ]
                [ Svg.rect (defaultRect size "stroke:green;line-style:dashed;fill:none") []
                , Svg.foreignObject (defaultRect size "fill:red") [ Html.body [] [ cell ] ]
                ]


offset : ( number, number ) -> Svg.Attribute b
offset ( x, y ) =
    SA.transform <| "translate (" ++ (toString x) ++ "," ++ (toString y) ++ ")"


defaultRect : Window.Size -> String -> List (Svg.Attribute a)
defaultRect size style =
    [ SA.x <| toString (-size.width // 2)
    , SA.y <| toString (-size.height // 2)
    , SA.width <| toString size.width
    , SA.height <| toString size.height
    , SA.style style
    ]


pageOffset : Model a -> ( Float, Float )
pageOffset model =
    let
        size =
            cellSize model
    in
        ( toFloat <| fst model.curPage * size.width
        , toFloat <| snd model.curPage * size.height
        )


add : ( number, number ) -> ( number, number ) -> ( number, number )
add ( a, b ) ( x, y ) =
    ( a + x, b + y )


oppositeOf : ( Int, Int ) -> ( Int, Int )
oppositeOf ( x, y ) =
    ( -x, -y )



--SHIFT


shiftCols : Model a -> Rows a
shiftCols model =
    let
        (Rows top mid bot) =
            model.rows
    in
        if (fst model.curDir == -1) then
            Rows top (shiftLeft mid) bot
        else
            Rows top (shiftRight mid) bot


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
            Cols newLeft newCenter newRight


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
            Cols newLeft newCenter newRight



-- animation : Animation (Window.Size -> Svg Msg)
-- zeroState : Window.Size -> Svg Msg
-- zeroState =
--     (\s -> Svg.circle [ SA.cx "0", SA.cy "0", SA.r <| toString s.width ] [])
-- toSvg : Float -> Window.Size -> Svg Msg
-- toSvg t { width, height } =
--     Svg.circle
--         [ SA.r <| toString <| 0.3 * t * (toFloat (min width height))
--         , SA.cx "0"
--         , SA.cy "0"
--         , SA.stroke "#000000"
--         , SA.fillOpacity "0"
--         , SA.strokeWidth "5"
--         ]
--         []
-- viewRows : Window.Size->{ rowCount:Int }->Int ->Rows a -> Svg a
-- viewRows size rowCount idx (Rows top mid bot) =
--     let rows = top ++ [mid] ++ bot
--     in Svg.g [] (List.indexedMap (viewCols size { colCount = List.length rows }) rows)
-- viewCell : String  -> Int -> Int -> Html a -> Svg Msg
-- viewCell style size idx cell =
--     Svg.g [ offsetBy (30 * idx), SA.style style ] [ App.map (\s -> NoOp) <| embedHtml size cell ]
