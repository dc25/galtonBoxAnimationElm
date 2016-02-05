module Balls where

import Time exposing (Time, every)
import Effects exposing (Effects)
import Graphics.Collage exposing (collage)
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


view : Signal.Address Action -> Model -> Html
view action model = 
  collage Config.width Config.height (List.map Ball.viewAsForm model.balls)
  |> fromElement



