module Balls where

import Time exposing (Time, every)
import Effects exposing (Effects)
import Color exposing (red)
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

type Action = Drop Time | Step

drop : Signal Action 
drop = Signal.map (\t -> Drop t) (every Config.dropInterval)

tick : Signal Action 
tick = Signal.map (\_ -> Step) (every Config.stepInterval)

update : Action -> Model -> (Model, Effects Action)
update action model = 
  let (updatedBalls, updatedBins) = 
    case action of
      Drop t -> 
        (Ball.init t :: model.balls, model.bins)


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

       peg = polygon [(0,0), (-4, -8), (4, -8)] |> filled red 

       apex = toFloat ((Config.height//2 ) - Config.headRoom)

   in List.map (\(x,y) -> move (Config.hscale*toFloat x,  apex - Config.vscale*toFloat y) peg) galtonCoords

drawBallBins = []

view : Signal.Address Action -> Model -> Html
view action model = 
  let ballForms = (List.map Ball.viewAsForm model.balls)
  in collage Config.width Config.height (ballForms ++ drawGaltonBox ++ drawBallBins) |> fromElement



