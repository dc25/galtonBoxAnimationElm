module Ball where

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Time exposing (..)
import Random exposing (..)
import Html exposing (Html, fromElement)

dt : Float
dt = 0.1

scale : Float
scale = 10

type alias Model = 
  { level : Int
  , shift : Int
  , seed : Seed
  }

init : Model
init = { level = 0
       , shift = 0
       , seed = initialSeed 34342
       }

view : Signal.Address Action -> Model -> Html
view _ model = 
    let position = (scale * toFloat (model.shift), -scale * toFloat (model.level))
    in collage 700 500 [ circle 8 |> filled blue |> move position ]
       |> fromElement


type Action = Step

update : Action -> Model -> Model
update _ model = let 
                   deltaShift = map (\b -> if b then 1 else -1) bool
                   (delta, seed) = generate deltaShift model.seed
               in { level=(model.level)+1, shift = model.shift+delta, seed=seed}

