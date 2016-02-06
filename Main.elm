import StartApp exposing (..)
import Window exposing (dimensions)
import Effects exposing (Effects)
import Time exposing (Time, every)
import Color exposing (Color, black, red, blue, green)
import Graphics.Collage exposing (collage, polygon, filled, move)
import Ball exposing (init, update, viewAsForm)
import Html exposing (Html, fromElement)
import Dict exposing (Dict)
import Config

type alias Model = 
  { balls : List Ball.Model
  , bins : Dict Int Int
  , dimensions : (Int,Int)
  }

init : Model
init =
  { balls = []
  , bins = Dict.empty
  , dimensions = (0,0)
  }

type Action = Drop Int | Tick (Int, Int)

drop : Signal Action 
drop = Signal.foldp (\_ c -> c+1) 0 (every Config.dropInterval)
     |> Signal.filter ((>) (Config.dropCount)) 0 
     |> Signal.map (\t -> Drop t) 

tick : Signal Action 
tick  = Signal.map Tick (Signal.sampleOn (every Config.stepInterval) dimensions)

colorCycle : Int -> Color
colorCycle i =
    case i % 3 of
        0 -> red
        1 -> blue
        _ -> green

update : Action -> Model -> (Model, Effects Action)
update action model = 
    case action of
      Drop indx -> 
        ({ model | balls = Ball.init indx (colorCycle indx) :: model.balls}, Effects.none)

      Tick dim -> 
        -- foldr to execute update, append to balls, replace bins
        let (updatedBalls, updatedBins) =
          List.foldr (\ball (ballList, bins) -> 
                         let (updatedBall, updatedBins) = Ball.update model.dimensions (ball, bins) 
                         in (updatedBall :: ballList, updatedBins))
                     ([], model.bins)
                     model.balls
        in ({ model | balls = updatedBalls, bins = updatedBins, dimensions = dim}, Effects.none)

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
view action model = 
  let dim = model.dimensions
      (width, height) = dim
      ballForms = (List.map (Ball.viewAsForm dim) model.balls)
  in collage width height (ballForms ++ drawGaltonBox dim) |> fromElement

app : StartApp.App Model
app = StartApp.start 
  { init = (init, Effects.none)
  , update = update
  , view = view
  , inputs = [ drop, tick ]
  }

main : Signal Html
main = app.html
