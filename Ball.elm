module Ball where

import Color exposing (blue)
import Graphics.Collage exposing (filled, move, Form, circle)
import Time exposing (Time)
import Dict exposing (Dict)
import Random exposing (Seed, bool, generate, initialSeed, map)
import Config 

type Model = Galton Int Int Seed | Falling Int Float Float | Landed Int Float

init : Time -> Model
init t = Galton 0 0 (initialSeed(truncate t))

viewAsForm : Model -> Form
viewAsForm model = 
  let dropLevel = toFloat (Config.height//2 - Config.headRoom)
      position = 
        case model of

          Galton level shift seed -> 
            (Config.scale * toFloat shift, dropLevel-Config.scale * toFloat level)

          Falling shift distance _ -> 
            (Config.scale * toFloat shift, dropLevel-Config.scale * toFloat (Config.levelCount)-distance)

          Landed shift distance -> 
            (Config.scale * toFloat shift, dropLevel-Config.scale * toFloat (Config.levelCount)-distance)

  in circle 3 |> filled blue |> move position 

update : (Model, Dict Int Int) -> (Model, Dict Int Int)
update (model, bins) = 
  case model of

    Galton level shift seed ->
      let deltaShift = map (\b -> if b then 1 else -1) bool
          (delta, newSeed) = generate deltaShift seed
          newShift = shift+delta
          newLevel = (level)+1
      in if (newLevel < Config.levelCount) then
           (Galton newLevel newShift newSeed, bins)
         else
           (Falling newShift 0 0, bins)

    Falling shift distance velocity -> 
      let newDistance = distance + velocity
      in if (newDistance < Config.maxDrop) then
           (Falling shift newDistance (velocity + 1), bins)
         else
           (Landed shift Config.maxDrop, bins)

    Landed _ _ -> (model, bins)

