import StartApp exposing (..)
import Window exposing (dimensions)
import Effects exposing (Effects)
import Time exposing (Time, every)
import Graphics.Collage exposing (collage, polygon, filled, move, Form, circle)
import Coin exposing (initCoin, updateCoin, viewAsForm, drawGaltonBox)
import Html exposing (Attribute, Html, fromElement, text, div, input, button)
import Html.Attributes exposing (placeholder, value, style, disabled)
import Html.Events exposing (on, targetValue, onClick)
import Dict exposing (Dict, get, insert)
import String exposing (toInt)
import Result exposing (withDefault)
import Color exposing (Color, black, red, blue, green)
import Random exposing (Seed, bool, generate, initialSeed, map)

-- Display related parameters.
hscale = 10.0
vscale = hscale * 2
topMargin = 30
bottomMargin = 30
levelCount = 12
dropCount = 90
coinDiameter = hscale/ 2.0

type Motion = Galton Int Int Seed | Falling Int Float Float Float | Landed Int Float

type alias Coin = 
  { motion : Motion
  , color : Color
  }

colorCycle : Int -> Color
colorCycle i =
    case i % 3 of
        0 -> red
        1 -> blue
        _ -> green

initCoin : Int -> Coin
initCoin indx = {motion = Galton 0 0 (initialSeed indx), color=colorCycle indx}

viewAsForm : (Int, Int) -> Coin -> Form
viewAsForm (_, height) coin = 
  let dropLevel = toFloat (height//2 - topMargin)
      (level, shift, distance) = 
        case coin.motion of
          Galton level shift seed -> (level, shift, 0)
          Falling shift distance _ _-> (levelCount, shift, distance)
          Landed shift distance -> (levelCount, shift, distance)
      floatShift = toFloat shift
      position = 
        (             hscale * floatShift
        , dropLevel - vscale * (toFloat level) - distance + coinDiameter / 2.0)

  in coinDiameter |> circle |> filled coin.color |> move position 

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

coinsInBin : Int -> Dict Int Int -> Int
coinsInBin binNumber bins = 
  case get binNumber bins of
    Nothing -> 0
    Just n -> n

addToBins : Int -> Dict Int Int -> Dict Int Int
addToBins binNumber bins = 
  insert binNumber (coinsInBin binNumber bins + 1) bins

updateCoin : (Int, Int) -> (Coin, Dict Int Int) -> (Coin, Dict Int Int)
updateCoin (_, height) (coin, bins) = 
  case coin.motion of
    Galton level shift seed ->
      let deltaShift = map (\b -> if b then 1 else -1) bool
          (delta, newSeed) = generate deltaShift seed
          newShift = shift+delta
          newLevel = (level)+1
      in if (newLevel < levelCount) then
           ({coin | motion = Galton newLevel newShift newSeed}, bins)
         else -- transition to falling
           let maxDrop = toFloat (height - topMargin - bottomMargin) - toFloat (levelCount) * vscale
               floor = maxDrop - toFloat (coinsInBin newShift bins) * (coinDiameter*2 + 1)
           in ({coin | motion = Falling newShift -((vscale)/2.0) 10 floor}, addToBins newShift bins)

    Falling shift distance velocity floor -> 
      let newDistance = distance + velocity
      in if (newDistance < floor) then
           ({coin | motion = Falling shift newDistance (velocity + 1) floor}, bins)
         else -- transtion to landed
           ({coin | motion = Landed shift floor}, bins)

    Landed _ _ -> (coin, bins)

type alias Model = 
  { coins : List Coin.Coin
  , bins : Dict Int Int
  , dimensions : (Int,Int)
  , dropCountString : String
  , dropCount : Int
  , started : Bool
  }

init : Model
init =
  { coins = []
  , bins = Dict.empty
  , dimensions = (500,600)
  , dropCountString = ""
  , dropCount = 0
  , started = False
  }

type Action = Drop Int | Tick | SetCountString String | Go

drop : Signal Action 
drop = Signal.map (\t -> Drop (truncate t)) (every 200)

tick : Signal Action 
tick  = Signal.map (\t -> Tick) (every 50)

update : Action -> Model -> (Model, Effects Action)
update action model = 
    case action of
      Go ->
        let dropCount = toInt model.dropCountString |> withDefault 0 
            started = dropCount > 0
            dropCountString = if (started) then "" else model.dropCountString
        in ({model | dropCount = dropCount, dropCountString = dropCountString, started = started}, Effects.none)

      SetCountString count -> 
        ({ model | dropCountString = count}, Effects.none)

      Drop n -> 
        if (model.started && model.dropCount > 0) then
            let newDropCount = model.dropCount - 1
            in ({ model | 
                  dropCount = newDropCount, 
                  started = newDropCount > 0,
                  coins = Coin.initCoin n :: model.coins}, Effects.none)
        else
           (model, Effects.none)

      Tick -> 
        -- foldr to execute update, append to coins, replace bins
        let (updatedCoins, updatedBins) =
          List.foldr (\coin (coinList, bins) -> 
                         let (updatedCoin, updatedBins) = Coin.updateCoin model.dimensions (coin, bins) 
                         in (updatedCoin :: coinList, updatedBins))
                     ([], model.bins)
                     model.coins
        in ({ model | coins = updatedCoins, bins = updatedBins}, Effects.none)

view : Signal.Address Action -> Model -> Html
view address model = 
  div []
    [ input
        [ placeholder "How many?"
        , let showString = if (model.started)
                           then toString model.dropCount
                           else model.dropCountString
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

     , let dim = model.dimensions
           (width, height) = dim
           coinForms = (List.map (Coin.viewAsForm dim) model.coins)
       in collage width height (coinForms ++ drawGaltonBox dim) |> fromElement 
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
