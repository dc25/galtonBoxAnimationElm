import StartApp exposing (..)
import Window exposing (dimensions)
import Effects exposing (Effects)
import Time exposing (Time, every)
import Color exposing (Color, black, red, blue, green)
import Graphics.Collage exposing (collage, polygon, filled, move, Form)
import Ball exposing (init, update, viewAsForm)
import Html exposing (Attribute, Html, fromElement, text, div, input, button)
import Html.Attributes exposing (placeholder, value, style)
import Html.Events exposing (on, targetValue, onClick)
import Dict exposing (Dict)
import String exposing (toInt)
import Result exposing (withDefault)
import Config

type alias Model = 
  { balls : List Ball.Model
  , bins : Dict Int Int
  , dimensions : (Int,Int)
  , dropCountString : String
  , dropCount : Int
  , started : Bool
  }

init : Model
init =
  { balls = []
  , bins = Dict.empty
  , dimensions = (500,600)
  , dropCountString = ""
  , dropCount = 0
  , started = False
  }

type Action = Drop Int | Tick | SetCountString String | Go

drop : Signal Action 
drop = Signal.map (\t -> Drop (truncate t)) (every 200)

tick : Signal Action 
tick  = Signal.map (\t -> Tick) (every 50)

colorCycle : Int -> Color
colorCycle i =
    case i % 3 of
        0 -> red
        1 -> blue
        _ -> green

update : Action -> Model -> (Model, Effects Action)
update action model = 
    case action of
      Go ->
        let dropCount = toInt model.dropCountString |> withDefault 0 
            started = dropCount > 0
            dropCountString = if (started) then "" else model.dropCountString
        in ({model | dropCount = dropCount, dropCountString = dropCountString, started = started}, Effects.none)

      SetCountString count -> 
        ({ model | dropCountString = count}, Effects.none)

      Drop n -> 
        if (model.started && model.dropCount > 0) then
            let newDropCount = model.dropCount - 1
            in ({ model | 
                  dropCount = newDropCount, 
                  started = newDropCount > 0,
                  balls = Ball.init n (colorCycle n) :: model.balls}, Effects.none)
        else
           (model, Effects.none)

      Tick -> 
        -- foldr to execute update, append to balls, replace bins
       let (updatedBalls, updatedBins) =
          List.foldr (\ball (ballList, bins) -> 
                         let (updatedBall, updatedBins) = Ball.update model.dimensions (ball, bins) 
                         in (updatedBall :: ballList, updatedBins))
                     ([], model.bins)
                     model.balls
        in ({ model | balls = updatedBalls, bins = updatedBins}, Effects.none)

drawGaltonBox : (Int, Int) -> List Form
drawGaltonBox (width, height) = 
   let levels = [0..Config.levelCount-1]
  
       -- doubles :
       -- [0,2,4,6,8...]
       doubles = List.map (\n -> 2 * n) levels

       -- sequences :
       -- [[0], [0,2], [0,2,4], [0,2,4,6], [0,2,4,6,8],...]
       sequences = case List.tail (List.scanl (::) [] (doubles)) of
         Nothing -> []
         Just ls -> ls

       -- galtonCoords :
       -- [                            (0,0), 
       --                       (-1,1),      (1,1), 
       --                (-2,2),       (0,2),      (2,2), 
       --         (-3,3),       (-1,3),      (1,3),      (3,3), 
       --  (-4,4),       (-2,4),       (0,4),      (2,4),      (4,4), ...]
       galtonCoords = 
         List.map2 
           (\ls level -> List.map (\n -> (n - level, level)) ls) 
           sequences 
           levels
         |> List.concat

       peg = polygon [(0,0), (-4, -8), (4, -8)] |> filled black 

       apex = toFloat ((height//2 ) - Config.topMargin)

   in List.map (\(x,y) -> move (Config.hscale*toFloat x,  apex - Config.vscale*toFloat y) peg) galtonCoords

view : Signal.Address Action -> Model -> Html
view address model = 
  div []
    ([ input
        [ placeholder "How many?"
        , let showString = if (model.started)
                           then toString model.dropCount
                           else model.dropCountString
          in value showString
        , on "input" targetValue (Signal.message address << SetCountString)
        , myStyle
        ]
        [],

       button
        [ onClick address Go ]
        [ text "GO!" ]
     ] ++ 
     [ 
       let dim = model.dimensions
           (width, height) = dim
           ballForms = (List.map (Ball.viewAsForm dim) model.balls)
       in collage width height (ballForms ++ drawGaltonBox dim) |> fromElement 
     ])

myStyle : Attribute
myStyle =
  style
    [ ("width", "10%")
    , ("height", "20px")
    , ("padding", "0 0 0 0")
    , ("font-size", "1em")
    , ("text-align", "left")
    ]

app : StartApp.App Model
app = StartApp.start 
  { init = (init, Effects.none)
  , update = update
  , view = view
  , inputs = [ drop , tick ]
  }

main : Signal Html
main = app.html
