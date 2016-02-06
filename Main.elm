import StartApp exposing (..)
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
  }

init : Model
init =
  { balls = []
  , bins = Dict.empty
  }

type Action = Drop Int | Step

drop : Signal Action 
drop = Signal.foldp (\_ c -> c+1) 0 (every Config.dropInterval)
     |> Signal.filter ((>) (Config.dropCount)) 0 
     |> Signal.map (\t -> Drop t) 

tick : Signal Action 
tick = Signal.map (\_ -> Step) (every Config.stepInterval)

colorCycle : Int -> Color
colorCycle i =
    case i % 3 of
        0 -> red
        1 -> blue
        _ -> green

update : Action -> Model -> (Model, Effects Action)
update action model = 
  let (updatedBalls, updatedBins) = 
    case action of
      Drop indx -> 
        (Ball.init indx (colorCycle indx) :: model.balls, model.bins)

      Step -> 
        -- foldr to execute update, append to balls, replace bins
        List.foldr 
          (\ball (ballList, bins) -> 
             let (updatedBall, updatedBins) = Ball.update (ball, bins) 
             in (updatedBall :: ballList, updatedBins))
          ([], model.bins)
          model.balls

  in ({ balls = updatedBalls, bins = updatedBins }, Effects.none)

drawGaltonBox = 
   let levels = [0..Config.levelCount-1]

       doubles = List.map (\n -> 2 * n) levels

       sequences = case List.tail (List.scanl (::) [] (doubles)) of
         Nothing -> []
         Just ls -> ls

       galtonCoords = 
         List.map2 
           (\ls level -> List.map (\n -> (n - level, level)) ls) 
           sequences 
           levels
         |> List.concat

       peg = polygon [(0,0), (-4, -8), (4, -8)] |> filled black 

       apex = toFloat ((Config.height//2 ) - Config.headRoom)

   in List.map (\(x,y) -> move (Config.hscale*toFloat x,  apex - Config.vscale*toFloat y) peg) galtonCoords

view : Signal.Address Action -> Model -> Html
view action model = 
  let ballForms = (List.map Ball.viewAsForm model.balls)
  in collage Config.width Config.height (ballForms ++ drawGaltonBox) |> fromElement








app : StartApp.App Model
app = StartApp.start 
  { init = (init, Effects.none)
  , update = update
  , view = view
  , inputs = [ drop, tick ]
  }

main : Signal Html
main = app.html
