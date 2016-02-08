module Ball where

import Color exposing (Color, black, red, blue, green)
import Graphics.Collage exposing (filled, move, Form, circle, polygon)
import Time exposing (Time)
import Dict exposing (Dict, get, insert)
import Random exposing (Seed, bool, generate, initialSeed, map)

-- Display related parameters.
hscale = 10.0
vscale = hscale * 2
topMargin = 30
bottomMargin = 30
levelCount = 12
dropCount = 90
ballDiameter = hscale/ 2.0

type Motion = Galton Int Int Seed | Falling Int Float Float Float | Landed Int Float

type alias Model = 
  { motion : Motion
  , color : Color
  }

colorCycle : Int -> Color
colorCycle i =
    case i % 3 of
        0 -> red
        1 -> blue
        _ -> green

init : Int -> Model
init indx = {motion = Galton 0 0 (initialSeed indx), color=colorCycle indx}

viewAsForm : (Int, Int) -> Model -> Form
viewAsForm (_, height) model = 
  let dropLevel = toFloat (height//2 - topMargin)
      (level, shift, distance) = 
        case model.motion of
          Galton level shift seed -> (level, shift, 0)
          Falling shift distance _ _-> (levelCount, shift, distance)
          Landed shift distance -> (levelCount, shift, distance)
      floatShift = toFloat shift
      position = 
        (             hscale * floatShift
        , dropLevel - vscale * (toFloat level) - distance + ballDiameter / 2.0)

  in ballDiameter |> circle |> filled model.color |> move position 

drawGaltonBox : (Int, Int) -> List Form
drawGaltonBox (width, height) = 
   let levels = [0..levelCount-1]
  
       -- doubles :
       -- [0,2,4,6,8...]
       doubles = List.map (\n -> 2 * n) levels

       -- sequences :
       -- [[0], [0,2], [0,2,4], [0,2,4,6], [0,2,4,6,8],...]
       sequences = case List.tail (List.scanl (::) [] (doubles)) of
         Nothing -> []
         Just ls -> ls

       -- galtonCoords :
       -- [                            (0,0), 
       --                       (-1,1),      (1,1), 
       --                (-2,2),       (0,2),      (2,2), 
       --         (-3,3),       (-1,3),      (1,3),      (3,3), 
       --  (-4,4),       (-2,4),       (0,4),      (2,4),      (4,4), ...]
       galtonCoords = 
         List.map2 
           (\ls level -> List.map (\n -> (n - level, level)) ls) 
           sequences 
           levels
         |> List.concat

       peg = polygon [(0,0), (-4, -8), (4, -8)] |> filled black 

       apex = toFloat ((height//2 ) - topMargin)

   in List.map (\(x,y) -> move (hscale*toFloat x,  apex - vscale*toFloat y) peg) galtonCoords

ballsInBin : Int -> Dict Int Int -> Int
ballsInBin binNumber bins = 
  case get binNumber bins of
    Nothing -> 0
    Just n -> n

addToBins : Int -> Dict Int Int -> Dict Int Int
addToBins binNumber bins = 
  insert binNumber (ballsInBin binNumber bins + 1) bins

update : (Int, Int) -> (Model, Dict Int Int) -> (Model, Dict Int Int)
update (_, height) (model, bins) = 
  case model.motion of
    Galton level shift seed ->
      let deltaShift = map (\b -> if b then 1 else -1) bool
          (delta, newSeed) = generate deltaShift seed
          newShift = shift+delta
          newLevel = (level)+1
      in if (newLevel < levelCount) then
           ({model | motion = Galton newLevel newShift newSeed}, bins)
         else -- transition to falling
           let maxDrop = toFloat (height - topMargin - bottomMargin) - toFloat (levelCount) * vscale
               floor = maxDrop - toFloat (ballsInBin newShift bins) * (ballDiameter*2 + 1)
           in ({model | motion = Falling newShift -((vscale)/2.0) 10 floor}, addToBins newShift bins)

    Falling shift distance velocity floor -> 
      let newDistance = distance + velocity
      in if (newDistance < floor) then
           ({model | motion = Falling shift newDistance (velocity + 1) floor}, bins)
         else -- transtion to landed
           ({model | motion = Landed shift floor}, bins)

    Landed _ _ -> (model, bins)

