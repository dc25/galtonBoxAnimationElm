module Ball where

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Time exposing (..)
import Random exposing (..)
import Effects exposing (Effects)
import Html exposing (Html, fromElement)

interval : Time
interval = 100

scale : Float
scale = 10

type Action = Step

tick : Signal Action 
tick = Signal.map (\_ -> Step) (every interval)

type alias Model = 
  { level : Int
  , shift : Int
  , seed : Seed
  }

init : Model
init = 
  { level = 0
  , shift = 0
  , seed = initialSeed 34342
  }

viewAsForm : Model -> Form
viewAsForm model = 
  let position = (scale * toFloat (model.shift), -scale * toFloat (model.level))
  in circle 8 |> filled blue |> move position 

view : Signal.Address Action -> Model -> Html
view action model = 
  collage 700 500 [ viewAsForm model ]
  |> fromElement

update : Action -> Model -> (Model, Effects Action)
update _ model = let 
                   deltaShift = map (\b -> if b then 1 else -1) bool
                   (delta, seed) = generate deltaShift model.seed
               in ( { level=(model.level)+1
                    , shift = model.shift+delta
                    , seed=seed}
                  , Effects.none)

