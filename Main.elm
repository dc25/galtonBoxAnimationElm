import StartApp exposing (..)
import Effects exposing (Effects)
import Time exposing (Time, every)
import Graphics.Collage exposing (collage, polygon, filled, move, Form, circle)
import Html exposing (Attribute, Html, fromElement, text, div, input, button)
import Html.Attributes exposing (placeholder, value, style, disabled)
import Html.Events exposing (on, targetValue, onClick)
import Dict exposing (Dict, get, insert)
import String exposing (toInt)
import Result exposing (withDefault)
import Color exposing (Color, black, red, blue, green)
import Random exposing (Seed, bool, generate, initialSeed, map)

-- Display related parameters.
width = 500
height = 600
hscale = 10.0
vscale = hscale * 2
margin = 30
levelCount = 12
radius = hscale/ 2.0

type Motion = Galton Int Int Seed | Falling Int Float Float Float | Landed Int Float

type Coin = Coin Motion Color

colorCycle : Int -> Color
colorCycle i =
  case i % 3 of
    0 -> red
    1 -> blue
    _ -> green

initCoin : Int -> Coin
initCoin indx = Coin (Galton 0 0 (initialSeed indx)) (colorCycle indx)

drawCoin : Coin -> Form
drawCoin (Coin motion color) = 
  let dropLevel = toFloat (height//2 - margin)
      (level, shift, distance) = 
        case motion of
          Galton level shift seed -> (level, shift, 0)
          Falling shift distance _ _-> (levelCount, shift, distance)
          Landed shift distance -> (levelCount, shift, distance)
      floatShift = toFloat shift
      position = 
        (             hscale * floatShift
        , dropLevel - vscale * (toFloat level) - distance + radius / 2.0)

  in radius |> circle |> filled color |> move position 

drawGaltonBox : List Form
drawGaltonBox = 
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

      apex = toFloat (height//2 - margin)

  in List.map (\(x,y) -> move (hscale*toFloat x,  apex - vscale*toFloat y) peg) galtonCoords

coinsInBin : Int -> Dict Int Int -> Int
coinsInBin binNumber bins = 
  case get binNumber bins of
    Nothing -> 0
    Just n -> n

addToBins : Int -> Dict Int Int -> Dict Int Int
addToBins binNumber bins = 
  insert binNumber (coinsInBin binNumber bins + 1) bins

updateCoin : (Coin, Dict Int Int) -> (Coin, Dict Int Int)
updateCoin (Coin motion color, bins) = 
  case motion of
    Galton level shift seed ->
      let deltaShift = map (\b -> if b then 1 else -1) bool
          (delta, newSeed) = generate deltaShift seed
          newShift = shift+delta
          newLevel = (level)+1
      in if (newLevel < levelCount) then
           (Coin (Galton newLevel newShift newSeed) color, bins)
         else -- transition to falling
           let maxDrop = toFloat (height - 2 * margin) - toFloat (levelCount) * vscale
               floor = maxDrop - toFloat (coinsInBin newShift bins) * (radius*2 + 1)
           in (Coin (Falling newShift -((vscale)/2.0) 10 floor) color, addToBins newShift bins)

    Falling shift distance velocity floor -> 
      let newDistance = distance + velocity
      in if (newDistance < floor) then
           (Coin (Falling shift newDistance (velocity + 1) floor) color, bins)
         else -- transtion to landed
           (Coin (Landed shift floor) color, bins)

    Landed _ _ -> (Coin motion color, bins)

type alias Model = 
  { coins : List Coin
  , bins : Dict Int Int
  , countString : String
  , count : Int
  , started : Bool
  }

init : Model
init =
  { coins = []
  , bins = Dict.empty
  , countString = ""
  , count = 0
  , started = False
  }

type Action = Drop Int | Tick | SetCountString String | Go

drop : Signal Action 
drop = Signal.map (\t -> Drop (truncate (t*1000))) (every 200)

tick : Signal Action 
tick  = Signal.map (\t -> Tick) (every 50)

update : Action -> Model -> (Model, Effects Action)
update action model = 
  case action of
    Go ->
      let count = toInt model.countString |> withDefault 0 
          started = count > 0
          countString = if (started) then "" else model.countString
      in ({model | count = count, countString = countString, started = started}, Effects.none)

    SetCountString count -> 
      ({ model | countString = count}, Effects.none)

    Drop n -> 
      if (model.started && model.count > 0) then
          let newcount = model.count - 1
          in ({ model | 
                count = newcount, 
                started = newcount > 0,
                coins = initCoin n :: model.coins}, Effects.none)
      else
         (model, Effects.none)

    Tick -> 
      -- foldr to execute update, append to coins, replace bins
      let (updatedCoins, updatedBins) =
        List.foldr (\coin (coinList, bins) -> 
                       let (updatedCoin, updatedBins) = updateCoin (coin, bins) 
                       in (updatedCoin :: coinList, updatedBins))
                   ([], model.bins)
                   model.coins
      in ({ model | coins = updatedCoins, bins = updatedBins}, Effects.none)

view : Signal.Address Action -> Model -> Html
view address model = 
  div []
    [ input
        [ placeholder "How many?"
        , let showString = if (model.started) -- count down if started
                           then toString model.count
                           else model.countString
          in value showString
        , on "input" targetValue (Signal.message address << SetCountString)
        , disabled model.started
        , style [ ("height", "20px") ]
        ]
        []

     , button
        [ onClick address Go 
        , disabled model.started
        , style [ ("height", "20px") ]
        ]
        [ text "GO!" ]

     , let coinForms = (List.map (drawCoin) model.coins)
       in collage width height (coinForms ++ drawGaltonBox) |> fromElement 
    ]

app : StartApp.App Model
app = StartApp.start 
  { init = (init, Effects.none)
  , update = update
  , view = view
  , inputs = [ drop , tick ]
  }

main : Signal Html
main = app.html
