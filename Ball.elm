module Ball where

import Color exposing (Color, blue)
import Graphics.Collage exposing (filled, move, Form, circle)
import Time exposing (Time)
import Dict exposing (Dict, get, insert)
import Random exposing (Seed, bool, generate, initialSeed, map)
import Config 

type Motion = Galton Int Int Seed | Falling Int Float Float Float | Landed Int Float

type alias Model = 
  { motion : Motion
  , color : Color
  }

init : Int -> Color -> Model
init indx c = {motion = Galton 0 0 (initialSeed indx), color=c}

viewAsForm : Model -> Form
viewAsForm model = 
  let dropLevel = toFloat (Config.height//2 - Config.headRoom)
      (level, shift, distance) = 
        case model.motion of
          Galton level shift seed -> (level, shift, 0)
          Falling shift distance _ _-> (Config.levelCount, shift, distance)
          Landed shift distance -> (Config.levelCount, shift, distance)
      floatShift = toFloat shift
      position = 
        (             Config.hscale * floatShift
        , dropLevel - Config.vscale * (toFloat level) - distance + Config.ballDiameter / 2.0)

  in Config.ballDiameter |> circle |> filled model.color |> move position 

ballsInBin : Int -> Dict Int Int -> Int
ballsInBin binNumber bins = 
  case get binNumber bins of
    Nothing -> 0
    Just n -> n

addToBins : Int -> Dict Int Int -> Dict Int Int
addToBins binNumber bins = 
  insert binNumber (ballsInBin binNumber bins + 1) bins

update : (Model, Dict Int Int) -> (Model, Dict Int Int)
update (model, bins) = 
  case model.motion of
    Galton level shift seed ->
      let deltaShift = map (\b -> if b then 1 else -1) bool
          (delta, newSeed) = generate deltaShift seed
          newShift = shift+delta
          newLevel = (level)+1
      in if (newLevel < Config.levelCount) then
           ({motion = Galton newLevel newShift newSeed, color=model.color}, bins)
         else -- transition to falling
           let floor = Config.maxDrop - toFloat (ballsInBin newShift bins) * (Config.ballDiameter + 1)* 2 
           in ({motion = Falling newShift -((Config.vscale)/2.0) 10 floor, color=model.color}, addToBins newShift bins)

    Falling shift distance velocity floor -> 
      let newDistance = distance + velocity
      in if (newDistance < floor) then
           ({motion = Falling shift newDistance (velocity + 1) floor, color=model.color}, bins)
         else -- transtion to landed
           ({motion = Landed shift floor, color=model.color}, bins)

    Landed _ _ -> (model, bins)

