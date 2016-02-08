import StartApp exposing (..)
import Window exposing (dimensions)
import Effects exposing (Effects)
import Time exposing (Time, every)
import Graphics.Collage exposing (collage, polygon, filled, move, Form)
import Coin exposing (init, update, viewAsForm, drawGaltonBox)
import Html exposing (Attribute, Html, fromElement, text, div, input, button)
import Html.Attributes exposing (placeholder, value, style, disabled)
import Html.Events exposing (on, targetValue, onClick)
import Dict exposing (Dict)
import String exposing (toInt)
import Result exposing (withDefault)

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
                  coins = Coin.init n :: model.coins}, Effects.none)
        else
           (model, Effects.none)

      Tick -> 
        -- foldr to execute update, append to coins, replace bins
        let (updatedCoins, updatedBins) =
          List.foldr (\coin (coinList, bins) -> 
                         let (updatedCoin, updatedBins) = Coin.update model.dimensions (coin, bins) 
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
