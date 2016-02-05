module Ball where

import Color exposing (blue)
import Graphics.Collage exposing (filled, move, Form, circle)
import Time exposing (Time)
import Random exposing (Seed, bool, generate, initialSeed, map)
import Effects exposing (Effects)
import Html exposing (Html, fromElement)
import Config 

type Action = Step

type alias GaltonModel = 
  { level : Int
  , shift : Int
  , seed : Seed
  }

init : Time -> Model
init t = 
  GModel { level = 0
  , shift = 0
  , seed = initialSeed (truncate t)
  }

type Model = GModel GaltonModel

viewAsForm : Model -> Form
viewAsForm (GModel model) = 
  let dropLevel = toFloat (Config.height//2 - Config.headRoom)
      position = (Config.scale * toFloat (model.shift), dropLevel-Config.scale * toFloat (model.level))
  in circle 3 |> filled blue |> move position 

update : Action -> Model -> (Model, Effects Action)
update _ (GModel model) = 
  let deltaShift = map (\b -> if b then 1 else -1) bool
      (delta, seed) = generate deltaShift model.seed
  in ( GModel { model | 
         level=(model.level)+1
       , shift = model.shift+delta
       , seed=seed}
     , Effects.none)

