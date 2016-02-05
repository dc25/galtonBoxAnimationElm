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

type alias FallingModel = 
  { shift : Int
  , velocity: Float
  }

type Model = GModel GaltonModel | FModel FallingModel

init : Time -> Model
init t = 
  GModel { level = 0
  , shift = 0
  , seed = initialSeed (truncate t)
  }


viewAsForm : Model -> Form
viewAsForm model = 
  case model of

    GModel gModel -> 
      let dropLevel = toFloat (Config.height//2 - Config.headRoom)
          position = (Config.scale * toFloat (gModel.shift), dropLevel-Config.scale * toFloat (gModel.level))
      in circle 3 |> filled blue |> move position 

    FModel fModel -> 
      circle 3 |> filled blue

update : Action -> Model -> (Model, Effects Action)
update _ model = 
  case model of

    GModel gModel ->
      let deltaShift = map (\b -> if b then 1 else -1) bool
          (delta, seed) = generate deltaShift gModel.seed
      in ( GModel { gModel | 
             level=(gModel.level)+1
           , shift = gModel.shift+delta
           , seed=seed}
         , Effects.none)

    FModel gModel ->
      (init 0, Effects.none)

