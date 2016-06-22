module Pane exposing (..)

import Svg exposing (Svg)
import Html exposing (Html)
import Html.App as App
import Svg.Attributes as SA
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
import Rows exposing (Dir, Rows, Cols, Cell)


type alias Page =
    ( Int, Int )


type alias Model a =
    { animation : Animation (Window.Size -> Dir -> ( Float, Float ))
    , rows : Rows a
    , windowSize : Window.Size
    , keyboardModel : Keyboard.Model
    , curDir : Dir
    , curPage : Page
    , msg : String
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


step1 : Rows a
step1 =
    Rows.Rows [] (Rows.Cols [] (fromText "center") []) []


step2 : Rows a
step2 =
    Rows.Rows [] (Rows.Cols [] (fromText "center") [ fromText "col-rightof-center" ]) []


step3 : Rows a
step3 =
    Rows.Rows [] (Rows.Cols [ fromText "col-leftof-center" ] (fromText "center") [ fromText "col-rightof-center" ]) []


step4 : Rows a
step4 =
    Rows.Rows [] (Rows.Cols [ fromText "col-leftof-center" ] (fromText "center") [ fromText "col-rightof-center" ]) [ Rows.Cols [] (fromText "below-center") [] ]


step5 : Rows a
step5 =
    Rows.Rows [ Rows.Cols [] (fromText "above-center") [] ] (Rows.Cols [ fromText "col-leftof-center" ] (fromText "center") [ fromText "col-rightof-center" ]) [ Rows.Cols [] (fromText "below-center") [] ]


emptyCols : Cols a
emptyCols =
    (Rows.Cols [] (Svg.g [] []) [])


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
        , rows = step5
        , animation = Animation.immediately zeroState
        , keyboardModel = keyboardModel
        , curDir = ( 0, 0 )
        , curPage = ( 0, 0 )
        , msg = ""
        }
            ! [ Cmd.map KeyboardMsg keyboardCmd, Window.size |> Task.performFailproof (\s -> OnSizeChanged s) ]


center : Model a -> Cell a
center model =
    let
        (Rows.Rows top (Rows.Cols left center right) bot) =
            model.rows
    in
        center


scrollAnimation : Animation (Window.Size -> Dir -> ( Float, Float ))
scrollAnimation =
    (0.5 * Time.second)
        |> Animation.interval
        |> Animation.map Ease.inOutBack
        |> Animation.map
            (\t size dir ->
                ( -t * (toFloat <| fst dir) * (toFloat <| size.width)
                , -t * (toFloat <| snd dir) * (toFloat <| size.height)
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
                    update (Scroll dir) { model | keyboardModel = keyboardModel, msg = toString dir }
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
                        { model | msg = "cannot shift to " ++ toString dir } ! []
                    else
                        { model
                            | animation = scrollAnimation
                            , curDir = dir
                            , msg = "from page: " ++ (toString model.curPage) ++ " animating to " ++ toString dir
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
                        let
                            curPage =
                                add model.curPage <| oppositeOf model.curDir
                        in
                            { model
                                | animation = Animation.immediately zeroState
                                , rows = Rows.shift model.rows model.curDir |> Debug.log "ffs"
                                , curPage = curPage
                                , curDir = ( 0, 0 )
                                , msg = "set page to " ++ toString curPage
                            }
                                ! []


zeroState : Window.Size -> Dir -> ( Float, Float )
zeroState s d =
    ( 0, 0 )



--(\s -> Svg.circle [ SA.cx "0", SA.cy "0", SA.r <| toString s.width ] [])


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
        { width = width // 4, height = height // 4 }


view : Model a -> Svg Msg
view m =
    let
        { width, height } =
            windowSize m

        (Rows.Rows top mid bot) =
            m.rows

        (Rows.Cols left center right) =
            mid

        size =
            cellSize m

        allRows =
            top ++ [ mid ] ++ bot

        maxCols =
            Debug.log "max" <| Rows.maxCols m.rows

        centerAdj =
            ( toFloat <| maxCols * size.width, toFloat <| List.length top * size.height )

        totOff =
            add centerAdj (pageOffset m)

        --totOff =  pageOffset m
        --(*) -0.5 <|
    in
        Html.div []
            [ Svg.svg
                [ SA.width <| toString width
                , SA.height <| toString height
                , SA.viewBox <| viewBox <| windowSize m
                , SA.style "border: 1px solid #cccccc;"
                ]
                [ Svg.g [ offset centerAdj ] (List.indexedMap (viewCol m (List.length allRows)) allRows)
                , Svg.circle [ SA.cx "0", SA.cy "0", SA.r "3", SA.fill "green" ] []
                ]
            , Html.div [] [ Html.text m.msg ]
            ]



--


viewCol : Model a -> Int -> Int -> Cols a -> Svg Msg
viewCol model rowCount idx (Rows.Cols left center right) =
    let
        all =
            List.reverse left ++ [ center ] ++ right

        size =
            cellSize model

        rowOffset =
            ( -size.width * (List.length left), (-rowCount + idx) * size.height )
    in
        Svg.g [ offset rowOffset ]
            <| (List.indexedMap (viewCell model (List.length all)) all)


viewCell : Model a -> Int -> Int -> Cell a -> Svg Msg
viewCell model colCount idx cell =
    let
        size =
            cellSize model

        colOff =
            ( toFloat <| (-colCount + idx) * size.width, 0 )

        animOff =
            Animation.sample model.animation size model.curDir

        --totOff = add colOff animOff
        --<| add animOff colOff
    in
        cellSvg size cell (add colOff animOff)


cellSvg size cell off =
    App.map (\_ -> NoOp)
        <| Svg.g [ offset off ]
            [ Svg.rect (defaultRect size "stroke:green;line-style:dashed;fill:none") []
            , Svg.foreignObject (defaultRect size "fill:red") [ Html.body [] [ cell ] ]
            ]



-- defaultRect : Window.Size -> String -> List (Svg.Attribute a)
-- defaultRect size style =
--     [ SA.x <| toString (size.width // 2)
--     , SA.y <| toString (size.height // 2)
--     , SA.width <| toString size.width
--     , SA.height <| toString size.height
--     , SA.style style
--     ]


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
        ( toFloat <| fst model.curPage * -size.width
        , toFloat <| snd model.curPage * -size.height
        )


add : ( number, number ) -> ( number, number ) -> ( number, number )
add ( a, b ) ( x, y ) =
    ( a + x, b + y )


oppositeOf : ( Int, Int ) -> ( Int, Int )
oppositeOf ( x, y ) =
    ( -x, -y )



-- animation : Animation (Window.Size -> Svg Msg)
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
