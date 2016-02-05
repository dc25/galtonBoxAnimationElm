module Ball where

import Color exposing (blue)
import Graphics.Collage exposing (filled, move, Form, circle)
import Time exposing (Time)
import Random exposing (Seed, bool, generate, initialSeed, map)
import Config 

type Model = GModel Int Int Seed | FModel Int Float Float

init : Time -> Model
init t = GModel 0 0 (initialSeed(truncate t))

viewAsForm : Model -> Form
viewAsForm model = 
  let dropLevel = toFloat (Config.height//2 - Config.headRoom)
      position = 
        case model of
          GModel level shift seed -> 
            (Config.scale * toFloat shift, dropLevel-Config.scale * toFloat level)
          FModel shift distance _ -> 
            (Config.scale * toFloat shift, dropLevel-Config.scale * toFloat (Config.levelCount)-distance)
  in circle 3 |> filled blue |> move position 

update : Model -> Model
update model = 
  case model of

    GModel level shift seed ->
      let deltaShift = map (\b -> if b then 1 else -1) bool
          (delta, newSeed) = generate deltaShift seed
          newShift = shift+delta
          newLevel = (level)+1
      in if (newLevel < Config.levelCount) then
           GModel newLevel newShift newSeed
         else
           FModel newShift 0 0

    FModel shift distance velocity -> 
      FModel shift (distance + velocity) (velocity + 1)

