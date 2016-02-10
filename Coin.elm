module Coin where 

import Graphics.Collage exposing (polygon, filled, move, Form, circle)
import Dict exposing (Dict, get, insert)
import Color exposing (Color, black, red, blue, green)
import Random.PCG as Random exposing (Seed, bool, generate, map)
import Const

type State = InBox Int Int Seed | Falling Int Float Float Float | Landed Int Float

type Coin = Coin State Color

colorCycle : Int -> Color
colorCycle i =
  case i % 3 of
    0 -> red
    1 -> blue
    _ -> green

initCoin : Int -> Seed -> Coin
initCoin indx seed = Coin (InBox 0 0 seed) (colorCycle indx)

drawCoin : Coin -> Form
drawCoin (Coin state color) = 
  let dropLevel = toFloat (Const.height//2 - Const.margin)
      (level, shift, distance) = 
        case state of
          InBox level shift seed -> (level, shift, 0)
          Falling shift distance _ _-> (Const.levelCount, shift, distance)
          Landed shift distance -> (Const.levelCount, shift, distance)
      position = 
        (             Const.hscale * toFloat shift
        , dropLevel - Const.vscale * (toFloat level) - distance + Const.radius / 2.0)

  in Const.radius |> circle |> filled color |> move position 

drawGaltonBox : List Form
drawGaltonBox = 
  let levels = [0..Const.levelCount-1]
 
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

      apex = toFloat (Const.height//2 - Const.margin)

  in List.map (\(x,y) -> move (Const.hscale*toFloat x,  apex - Const.vscale*toFloat y) peg) galtonCoords

coinsInBin : Int -> Dict Int Int -> Int
coinsInBin binNumber bins = 
  case get binNumber bins of
    Nothing -> 0
    Just n -> n

addToBins : Int -> Dict Int Int -> Dict Int Int
addToBins binNumber bins = 
  insert binNumber (coinsInBin binNumber bins + 1) bins

updateCoin : (Coin, Dict Int Int) -> (Coin, Dict Int Int)
updateCoin (Coin state color as coin, bins) = 
  case state of
    InBox level shift seed ->
      let deltaShift = map (\b -> if b then 1 else -1) bool
          (delta, newSeed) = generate deltaShift seed
          newShift = shift+delta
          newLevel = (level)+1
      in if (newLevel < Const.levelCount) then
           (Coin (InBox newLevel newShift newSeed) color, bins)
         else -- transition to falling
           let maxDrop = toFloat (Const.height - 2 * Const.margin) - toFloat (Const.levelCount) * Const.vscale
               floor = maxDrop - toFloat (coinsInBin newShift bins) * (Const.radius*2 + 1)
           in (Coin (Falling newShift -((Const.vscale)/2.0) 10 floor) color, addToBins newShift bins)

    Falling shift distance velocity floor -> 
      let newDistance = distance + velocity
      in if (newDistance < floor) then
           (Coin (Falling shift newDistance (velocity + 1) floor) color, bins)
         else -- transtion to landed
           (Coin (Landed shift floor) color, bins)

    Landed _ _ -> (coin, bins) -- unchanged



