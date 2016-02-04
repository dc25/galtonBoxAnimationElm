module Balls where

import Time exposing (Time, every)
import Effects exposing (Effects)
import Graphics.Collage exposing (collage)
import Ball exposing (init, update, viewAsForm)
import Html exposing (Html, fromElement)

dropInterval : Time
dropInterval = 100

stepInterval : Time
stepInterval = 50

type alias Model = 
  { balls : List Ball.Model
  }

init : Model
init =
  { balls = []
  }

type Action = Drop Time | Step

drop : Signal Action 
drop = Signal.map (\t -> Drop t) (every dropInterval)

tick : Signal Action 
tick = Signal.map (\_ -> Step) (every stepInterval)

update : Action -> Model -> (Model, Effects Action)
update action model = 
  case action of
    Drop t -> 
      let updatedBalls = Ball.init t :: model.balls
      in ({ balls = updatedBalls }, Effects.none)
    Step -> 
      let updatedBalls = List.map (fst << (Ball.update Ball.Step)) model.balls
      in ({ balls = updatedBalls }, Effects.none)


view : Signal.Address Action -> Model -> Html
view action model = 
  collage 700 1000 (List.map Ball.viewAsForm model.balls)
  |> fromElement



