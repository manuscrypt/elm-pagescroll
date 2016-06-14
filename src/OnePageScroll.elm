module OnePageScroll exposing (..)

import Html exposing ( Html)
import Html.App as App
import Html.Attributes as HA 
import Html.Events as HE 
import Animation exposing( Animation )
import AnimationFrame
import Time exposing( Time, second )
import Keyboard.Extra as Keyboard exposing ( Direction )
import Window
import Task
import Dict exposing (Dict)
import Util exposing (fromIntRecord, multiplyVec)
import Math.Vector2 as Vec2 exposing (Vec2, vec2, getX, getY, scale)
import Ease exposing (outQuint)

type alias Position = {x: Int, y: Int}

type alias Cell a =
    { html: Html a
    , pos: Position
    }

type alias Model a = 
    { cells: Dict Position (Cell a)
    , curCell: Position
    , nextCell: Maybe Position
    , animation: Animation Vec2
    , dir: Keyboard.Direction
    , pixelPos: Position
    , windowSize: Vec2
    , keyboardModel : Keyboard.Model
    }

type Axis
    = LeftRight
    | UpDown

type Msg 
    = NoOp
    | WindowMsg Window.Size
    | Animate Time
    | KeyboardMsg Keyboard.Msg
    | Scroll Keyboard.Direction

origin = vec2 0 0
defaultPos = { x = 0, y = 0 }
sizeToMsg s = WindowMsg s
defaultCell = 
    { html = Html.h1 [] [Html.text "Error"] 
    , pos = defaultPos
    } 

init: List (Cell a)->(Model a, Cmd Msg)
init pages =
    let ( keyboardModel, keyboardCmd ) = Keyboard.init
    in { cells = Dict.empty
       , curCell = defaultPos
       , nextCell = Nothing
       , animation = Animation.immediately origin
       , dir = Keyboard.NoDirection
       , pixelPos = defaultPos
       , keyboardModel = keyboardModel
       , windowSize = origin
    } ! [ Cmd.map KeyboardMsg keyboardCmd, initialSizeCmd ]

initialSizeCmd : Cmd Msg
initialSizeCmd =
  Task.perform (\_ -> NoOp) sizeToMsg Window.size


update: Msg->Model a->(Model a, Cmd Msg)
update msg model =
    case msg of 
        NoOp -> 
            (model, Cmd.none)

        Animate dt ->
            let moved = move model 
                animated = { moved | animation = Animation.run dt moved.animation }
            in case Animation.isDone animated.animation of
                True -> 
                    { animated | curCell = Maybe.withDefault defaultPos <| animated.nextCell, nextCell = Nothing } ! []
                False -> 
                     animated  ! []

        WindowMsg size ->
            let pos = 
                if model.windowSize == origin then
                    defaultPos
                else 
                    model.pixelPos

            in { model | windowSize = Util.fromIntTuple (size.width, size.height), pixelPos = pos } ! []

        KeyboardMsg keyMsg ->
            let
                ( keyboardModel, keyboardCmd ) = 
                    Keyboard.update keyMsg model.keyboardModel
                dir = Keyboard.arrowsDirection keyboardModel 

                (model',cmd) = update (Scroll dir) { model  | keyboardModel = keyboardModel }

            in  model' ! [ Cmd.map KeyboardMsg keyboardCmd, cmd ]

        Scroll dir -> 
            if dir == Keyboard.NoDirection || dir == model.dir then 
                model ! [] 
            else 
                if not(Animation.isDone model.animation) then model ! [] 
                else 
                    let dirVec = Util.fromIntTuple <| dirToTuple dir
                        tDir = Vec2.add (fromIntRecord model.curCell) dirVec 
                        animation = 0.6
                                    |> (*) Time.second
                                    |> Animation.interval
                                    |> Animation.map (\t -> scale t  (Util.multiplyVec dirVec model.windowSize))
                
                    in  Debug.log "anim" { model | dir = dir, animation = animation, nextCell = Just <|  Util.toIntRecord tDir  } ! [] 

sgn: Direction -> Float
sgn dir = 
    case dir of 
        Keyboard.North -> -1
        Keyboard.East -> 1
        Keyboard.South -> 1
        Keyboard.West -> -1
        _ -> 0

move: Model a->Model a
move model = 
    let v = Debug.log "sample" <| Animation.sample model.animation
        pagePos = Util.multiplyVec (Util.fromIntRecord model.curCell) model.windowSize
    in { model | pixelPos = { x= round <| getX pagePos + getX v
                            , y= round <| getY pagePos + getY v
                            }
       }

view: Model a -> Html Msg
view model = 
    Html.div [ scrollStyle model
             , HE.onClick (Scroll Keyboard.North)
             ]  [ Html.div [ HA.style [ (,) "visibility" "visible" ] ] [ Html.text "This is a test" ] 
                ]

scrollStyle: Model a -> Html.Attribute msg
scrollStyle model = 
    HA.style [ (,) "position" "absolute"   
             , (,) "width" "920px"   
             , (,) "margin" "auto"   
--             , (,) "visibility" "hidden"
--             , (,) "overflow-y" "scroll"
             , (,) "left" <| toString model.pixelPos.x ++ "px"
             , (,) "top" <| toString model.pixelPos.y ++ "px"
             , (,) "border" "solid 1px black"
             , (,) "padding-right" "50px"
             , (,) "padding-bottom" "50px"
             ]        

dirToTuple : Direction -> (Int,Int)
dirToTuple dir =
    case dir of
        Keyboard.North ->
            ( 0, 1 )
        Keyboard.NorthEast ->
            ( 1, 1 )
        Keyboard.East ->
            ( 1, 0 )
        Keyboard.SouthEast ->
            ( 1, -1 )
        Keyboard.South ->
            ( 0, -1 )
        Keyboard.SouthWest ->
            ( -1, -1 )
        Keyboard.West ->
            ( -1, 0 )
        Keyboard.NorthWest ->
            ( -1, 1 )
        Keyboard.NoDirection ->
            (0, 0)




subscriptions : { b | animation : Animation a } -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map KeyboardMsg Keyboard.subscriptions
        , Window.resizes sizeToMsg
        ,   if Animation.isDone model.animation then
                Sub.none
            else 
                AnimationFrame.diffs Animate
        ]

main : Program Never
main =
    App.program
        { init = init []
        , view = view
        , update = update
        , subscriptions = subscriptions
        }