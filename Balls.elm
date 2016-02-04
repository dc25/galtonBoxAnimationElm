module Balls where

import Ball exposing (init, viewAsForm)

interval : Time
interval = 1000

type alias Model = {
  balls : List Ball.Model
}

init : Model
init =
  { balls = []
  }

type Action = Drop, Step

release : Signal Action 
release = Signal.map (\_ -> Drop) (every interval)

tick : Signal Action 
tick = Signal.map (\_ -> Step) (every interval)

update : Action -> Model -> (Model, Effects Action)
update action model = 
  case action of
    Drop -> 
      let updatedBalls = Ball.init :: model.balls
      in { balls = updatedBalls }
    Step -> 
      let updatedBalls = List.map (fst.(Ball.update Ball.Step)) model.balls
      in { balls = updatedBalls }


view : Signal.Address Action -> Model -> Html
view action model = 
  collage 700 500 (List.map Ball.viewAsForm model.balls)
  |> fromElement



