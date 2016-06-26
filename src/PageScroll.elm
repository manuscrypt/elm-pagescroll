module PageScroll exposing (..)

import Html exposing (Html)
import VirtualDom
import Html.Attributes as HA
import Html.Events as HE
import Html.App as App
import Svg exposing (Svg)
import Svg.Attributes as SA
import String
import Dict exposing (Dict)
import Time exposing (Time)
import Json.Decode as Json exposing ((:=))
import Window
import AnimationFrame
import Animation exposing (Animation)
import Ease exposing (outQuint, inOutBack)
import Keyboard.Extra as Keyboard exposing (Direction)
import Task.Extra as Task


type alias Dir =
    { x : Float, y : Float }


type alias FloatWindowSize =
    { width : Float, height : Float }


type alias Offset =
    ( Float, Float )


type alias Cell a =
    VirtualDom.Node a


type Cols a
    = Cols (List (Cell a)) (Cell a) (List (Cell a))


type Rows a
    = Rows (List (Cols a)) (Cols a) (List (Cols a))


type alias Model a =
    { animation : Animation (FloatWindowSize -> Dir -> ( Float, Float ))
    , rows : Rows a
    , windowSize : FloatWindowSize
    , keyboardModel : Keyboard.Model
    , curDir : Dir
    }


type Msg a
    = Animate Time
    | OnSizeChanged Window.Size
    | KeyboardMsg Keyboard.Msg
    | Scroll Dir
    | Wheel Float
    | PageMsg a


noDir : Dir
noDir =
    { x = 0, y = 0 }


left : Dir
left =
    { x = -1, y = 0 }


right : Dir
right =
    { x = 1, y = 0 }


up : Dir
up =
    { x = 0, y = 1 }


down : Dir
down =
    { x = 0, y = -1 }


subscriptions : Model a -> Sub (Msg a)
subscriptions model =
    Sub.batch
        [ Sub.map KeyboardMsg Keyboard.subscriptions
        , Window.resizes (\size -> OnSizeChanged size)
        , if Animation.isDone model.animation then
            Sub.none
          else
            AnimationFrame.diffs Animate
        ]


init : Rows a -> ( Model a, Cmd (Msg a) )
init content =
    let
        ( keyboardModel, keyboardCmd ) =
            Keyboard.init
    in
        { windowSize = { width = 0, height = 0 }
        , rows = content
        , animation = Animation.immediately zeroState
        , keyboardModel = keyboardModel
        , curDir = noDir
        }
            ! [ Cmd.map KeyboardMsg keyboardCmd, Window.size |> Task.performFailproof (\s -> OnSizeChanged s) ]


scrollAnimation : Animation (FloatWindowSize -> Dir -> Offset)
scrollAnimation =
    (0.7 * Time.second)
        |> Animation.interval
        |> Animation.map Ease.outQuint
        |> Animation.map
            (\t size { x, y } ->
                ( -t * x * size.width
                , t * y * size.height
                )
            )


update : Msg a -> Model a -> ( Model a, Cmd (Msg a) )
update msg model =
    case msg of
        Wheel yOff ->
            if yOff > 0 then
                update (Scroll up) model
            else if yOff < 0 then
                update (Scroll down) model
            else
                model ! []

        KeyboardMsg keyMsg ->
            let
                ( keyboardModel, keyboardCmd ) =
                    Keyboard.update keyMsg model.keyboardModel

                { x, y } =
                    Keyboard.arrows keyboardModel

                ( model', cmd ) =
                    update (Scroll { x = toFloat x, y = toFloat y }) { model | keyboardModel = keyboardModel }
            in
                model' ! [ Cmd.map KeyboardMsg keyboardCmd, cmd ]

        Scroll dir ->
            if not <| Animation.isDone model.animation then
                model ! []
            else if shift model.rows dir == model.rows then
                model ! []
            else
                { model
                    | animation = scrollAnimation
                    , curDir = dir
                }
                    ! []

        OnSizeChanged size ->
            { model | windowSize = { width = toFloat size.width, height = toFloat size.height } } ! []

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
                            , rows = shift model.rows model.curDir
                            , curDir = noDir
                        }
                            ! []

        PageMsg _ ->
            model ! []


zeroState : FloatWindowSize -> Dir -> Offset
zeroState s d =
    ( 0, 0 )


view : Model a -> Svg (Msg a)
view m =
    let
        { width, height } =
            m.windowSize

        (Rows top mid bot) =
            m.rows

        allRows =
            List.reverse top ++ [ mid ] ++ bot
    in
        Html.body
            [ HA.style [ (,) "overflow" "hidden", (,) "overflow-x" "hidden", (,) "overflow-y" "hidden", (,) "-ms-overflow-style" "none", (,) "display" "block" ]
            ]
            [ Svg.svg
                [ SA.width <| toString width
                , SA.height <| toString height
                , SA.viewBox <| String.join " " <| List.map toString [ -width / 2, -height / 2, width, height ]
                , onMouseWheel Wheel
                ]
                (List.indexedMap (viewCol m (List.length top)) allRows)
            ]


viewCol : Model a -> Int -> Int -> Cols a -> Svg (Msg a)
viewCol model rowCount idx (Cols left center right) =
    let
        off =
            ( 0, (toFloat (-rowCount + idx)) * model.windowSize.height )
    in
        let
            cols =
                List.indexedMap (viewCell model (List.length left)) <| List.reverse left ++ [ center ] ++ right
        in
            Svg.g [ offset off ] cols


viewCell : Model a -> Int -> Int -> Cell a -> Svg (Msg a)
viewCell model colCount idx cell =
    let
        animOff =
            Animation.sample model.animation model.windowSize model.curDir

        off =
            ( (toFloat <| (-colCount + idx)) * model.windowSize.width, 0 )
    in
        cellSvg model.windowSize cell (add animOff off)


nsXHtml : String
nsXHtml =
    "http://www.w3.org/1999/xhtml/"


cellSvg : FloatWindowSize -> Html a -> Offset -> Html (Msg a)
cellSvg size cell off =
    App.map PageMsg
        <| Svg.foreignObject
                [ SA.x <| toString (-size.width / 2)
                , SA.y <| toString (-size.height / 2)
                , SA.width <| toString size.width
                , SA.height <| toString size.height
                , offset off 
                --, SA.requiredExtensions nsXHtml
                ]
                [ Html.body
                    [ HA.attribute "xmlns" nsXHtml
                    , HA.style
                        [ (,) "width" "100%"
                        , (,) "height" "100%"
                        ]
                    ]
                    [ cell ]
                ]
            

xx: {x: Float, y: Float} -> Int
xx dir = 
   case dir of 
       zedro -> 1

add : Offset -> Offset -> Offset
add ( a, b ) ( x, y ) =
    ( a + x, b + y )


offset : Offset -> Svg.Attribute b
offset ( x, y ) =
    SA.transform <| "translate (" ++ (toString x) ++ "," ++ (toString y) ++ ")"

shift : Rows a -> Dir -> Rows a
shift (Rows top mid bot) dir =
    if dir == left then
        Rows top (shiftLeft mid) bot
    else if dir == right then
        Rows top (shiftRight mid) bot
    else if dir == up then
        shiftUp <| Rows top mid bot
    else if dir == down then
        shiftDown <| Rows top mid bot
    else
        Rows top mid bot


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


onMouseWheel : (Float -> a) -> Html.Attribute a
onMouseWheel msg =
    HE.onWithOptions "wheel" { stopPropagation = True, preventDefault = True } (Json.map msg decodeWheelEvent)


decodeWheelEvent : Json.Decoder Float
decodeWheelEvent =
    Json.oneOf
        [ Json.at [ "deltaY" ] Json.float
        , Json.at [ "wheelDelta" ] Json.float |> Json.map (\v -> -v)
        ]
