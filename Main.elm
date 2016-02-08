import StartApp exposing (..)
import Effects exposing (Effects)
import Time exposing (Time, every)
import Graphics.Collage exposing (collage)
import Html exposing (Attribute, Html, fromElement, text, div, input, button)
import Html.Attributes exposing (placeholder, value, style, disabled)
import Html.Events exposing (on, targetValue, onClick)
import Dict exposing (Dict)
import String exposing (toInt)
import Result exposing (withDefault)
import Coin exposing (Coin, initCoin, updateCoin, drawCoin, drawGaltonBox)
import Const

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
