module Ball where

import Color exposing (blue)
import Graphics.Collage exposing (filled, move, Form, circle)
import Time exposing (Time)
import Random exposing (Seed, bool, generate, initialSeed, map)
import Effects exposing (Effects)
import Html exposing (Html, fromElement)
import Config 

type Action = Step

type alias Model = 
  { velocity : Float
  , position : Float
  , level : Float
  , shift : Int
  , seed : Seed
  }

init : Time -> Model 
init t = 
  { velocity = 0.0
  , position = 0.0
  , level = 0.0
  , shift = 0
  , seed = initialSeed (truncate t)
  }

viewAsForm : Model -> Form
viewAsForm model = 
  let dropLevel = toFloat (Config.height//2 - Config.headRoom)
      position = (Config.scale * toFloat (model.shift), dropLevel-Config.scale * model.level)
  in circle 3 |> filled blue |> move position 

update : Action -> Model -> (Model, Effects Action)
update _ model = 
  let deltaShift = map (\b -> if b then 1 else -1) bool
      (delta, seed) = generate deltaShift model.seed
  in ( { model | 
         level=(model.level)+1.0
       , shift = model.shift+delta
       , seed=seed}
     , Effects.none)

