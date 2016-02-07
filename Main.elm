import StartApp exposing (..)
import Window exposing (dimensions)
import Effects exposing (Effects)
import Time exposing (Time, every)
import Color exposing (Color, black, red, blue, green)
import Graphics.Collage exposing (collage, polygon, filled, move, Form)
import Ball exposing (init, update, viewAsForm)
import Html exposing (Attribute, Html, fromElement, text, div, input)
import Html.Attributes exposing (placeholder, value, style)
import Html.Events exposing (on, targetValue)
import Dict exposing (Dict)
import String
import Config

type alias Model = 
  { balls : List Ball.Model
  , bins : Dict Int Int
  , dimensions : (Int,Int)
  , dropCount : Int
  }

init : Model
init =
  { balls = []
  , bins = Dict.empty
  , dimensions = Config.dimensions
  , dropCount = 75
  }

type Action = Drop Int | Tick | SetCount Int

drop : Int -> Signal Action 
drop count = 
  Signal.foldp (\_ c -> c+1) 0 (every Config.dropInterval)
  |> Signal.filter ((>) count) 0 
  |> Signal.map (\t -> Drop t) 

tick : Signal Action 
-- tick  = Signal.map Tick (Signal.sampleOn (every Config.stepInterval) dimensions)
tick  = Signal.map (\t -> Tick) (every Config.stepInterval)

colorCycle : Int -> Color
colorCycle i =
    case i % 3 of
        0 -> red
        1 -> blue
        _ -> green

update : Action -> Model -> (Model, Effects Action)
update action model = 
    case action of
      SetCount count -> 
        ({ model | dropCount = count}, Effects.none)
      Drop indx -> 
        ({ model | balls = Ball.init indx (colorCycle indx) :: model.balls}, Effects.none)

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
  
       -- [0,2,4,6,8...]
       doubles = List.map (\n -> 2 * n) levels

       -- [[0], [0,2], [0,2,4], [0,2,4,6], [0,2,4,6,8],...]
       sequences = case List.tail (List.scanl (::) [] (doubles)) of
         Nothing -> []
         Just ls -> ls

       -- [(0,0), (-1,1),(1,1), (-2,2),(0,2),(2,2), (-3,3),(-1,3),(1,3),(3,3), (-4,4),(-2,4),(0,4),(2,4),(4,4),...]
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
        , value (toString model.dropCount)
        , on "input" targetValue (Signal.message address << SetCount << Result.withDefault 0 << String.toInt)
        , myStyle
        ]
        []
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
  , inputs = [ drop init.dropCount, tick ]
  }

main : Signal Html
main = app.html
