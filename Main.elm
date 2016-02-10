import StartApp exposing (..)
import Effects exposing (Effects)
import Time exposing (Time, every)
import Graphics.Collage exposing (collage)
import Html exposing (Attribute, Html, fromElement, text, div, input, button)
import Html.Attributes as A exposing (type', min, placeholder, value, style, disabled)
import Html.Events exposing (on, targetValue, onClick)
import Dict exposing (Dict)
import String exposing (toInt)
import Result exposing (withDefault)
import Coin exposing (Coin, initCoin, updateCoin, drawCoin, drawGaltonBox)
import Random.PCG as Random exposing (Seed, initialSeed, split)
import Const

type alias Model = 
  { coins : List Coin
  , bins : Dict Int Int
  , count : Int
  , started : Bool
  , seedInitialized : Bool
  , seed : Seed
  }

init : Model
init =
  { coins = []
  , bins = Dict.empty
  , count = 0
  , started = False
  , seedInitialized = False
  , seed = initialSeed 45 -- This will not get used.  Actual seed used is time dependent and set when the first coin drops.
  }

type Action = Drop Int | Tick | SetCount String | Go

drop : Signal Action 
drop = Signal.map (\t -> Drop (round t)) (every 200)

tick : Signal Action 
tick  = Signal.map (\t -> Tick) (every 40)

update : Action -> Model -> (Model, Effects Action)
update action model = 
  case action of
    Go ->
      ({model | started = model.count > 0}, Effects.none)

    SetCount countString -> 
      ({ model | count = toInt countString |> withDefault 0 }, Effects.none)

    Drop t -> 
      if (model.started && model.count > 0) then
          let newcount = model.count - 1
              seed' =  if model.seedInitialized then model.seed else initialSeed t
              (seed'', coinSeed) = split seed'
          in ({ model  
              | coins = initCoin t :: model.coins
              , count = newcount
              , started = newcount > 0
              , seedInitialized = True
              , seed = seed'}, Effects.none)
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
        , let showString = if model.count > 0 then model.count |> toString else ""
          in value showString
        , on "input" targetValue (Signal.message address << SetCount)
        , disabled model.started
        , style [ ("height", "20px") ]
        , type' "number"
        , A.min "1"
        ]
        []

     , button
        [ onClick address Go 
        , disabled model.started
        , style [ ("height", "20px") ]
        ]
        [ text "GO!" ]

     , let coinForms = (List.map (drawCoin) model.coins)
       in collage Const.width Const.height (coinForms ++ drawGaltonBox) |> fromElement 
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
