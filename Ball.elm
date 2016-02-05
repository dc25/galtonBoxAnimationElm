module Ball where

import Color exposing (blue)
import Graphics.Collage exposing (filled, move, Form, circle)
import Time exposing (Time)
import Random exposing (Seed, bool, generate, initialSeed, map)
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
  , distance: Float
  , velocity: Float
  }

type Model = GModel GaltonModel | FModel FallingModel

init : Time -> Model
init t = 
  GModel 
    { level = 0
    , shift = 0
    , seed = initialSeed (truncate t)
    }


viewAsForm : Model -> Form
viewAsForm model = 
  let dropLevel = toFloat (Config.height//2 - Config.headRoom)
  in case model of

       GModel gModel -> 
         let position = (Config.scale * toFloat (gModel.shift), dropLevel-Config.scale * toFloat (gModel.level))
         in circle 3 |> filled blue |> move position 

       FModel fModel -> 
         let position = (Config.scale * toFloat (fModel.shift), dropLevel-Config.scale * toFloat (Config.levelCount)-fModel.distance)
         in circle 3 |> filled blue |> move position

update : Action -> Model -> Model
update _ model = 
  case model of

    GModel gModel ->
      let deltaShift = map (\b -> if b then 1 else -1) bool
          (delta, newSeed) = generate deltaShift gModel.seed
          newShift = gModel.shift+delta
          newLevel = (gModel.level)+1
      in if (newLevel < Config.levelCount) then
           GModel 
             { level= newLevel
             , shift = newShift
             , seed=newSeed
             }
         else
           FModel 
             { shift = newShift
             , distance = 0
             , velocity = 0
             }

    FModel fModel -> 
      FModel 
        { fModel | 
          distance = fModel.distance + fModel.velocity
        , velocity = fModel.velocity + 1
        }

