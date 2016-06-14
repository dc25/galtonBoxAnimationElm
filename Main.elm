import Html.App exposing (program)
import Time exposing (Time, every, millisecond)
import Collage exposing (..)
import Element exposing (..)
import Html exposing (Attribute, Html, text, div, input, button)
import Html.Attributes as A exposing (type', min, placeholder, value, style, disabled)
import Html.Events exposing (onInput, targetValue, onClick)
import Dict exposing (Dict)
import String exposing (toInt)
import Result exposing (withDefault)
import Coin exposing (Coin, initCoin, updateCoin, drawCoin, drawGaltonBox)
import Random.Pcg as Random exposing (Seed, initialSeed, independentSeed, step)

import Const

type alias Model = 
  { coins : List Coin
  , bins : Dict Int Int
  , count : Int
  , started : Bool
  , seedInitialized : Bool
  , seed : Seed
  }

init : (Model, Cmd Msg)
init =
  ( { coins = []
    , bins = Dict.empty
    , count = 0
    , started = False
    , seedInitialized = False
    , seed = initialSeed 45 -- This will not get used.  Actual seed used is time dependent and set when the first coin drops.
    }, Cmd.none)

type Msg = Drop Time | Tick Time | SetCount String | Go

update : Msg -> Model -> (Model, Cmd Msg)
update action model = 
  case action of
    Go ->
      ({model | started = model.count > 0}, Cmd.none)

    SetCount countString -> 
      ({ model | count = toInt countString |> withDefault 0 }, Cmd.none)

    Drop t -> 
      if (model.started && model.count > 0) then
          let newcount = model.count - 1
              seed' =  if model.seedInitialized then model.seed else initialSeed (truncate t)
              (seed'', coinSeed) = step independentSeed seed'
          in ({ model  
              | coins = initCoin (truncate t) coinSeed :: model.coins
              , count = newcount
              , started = newcount > 0
              , seedInitialized = True
              , seed = seed''}, Cmd.none)
      else
         (model, Cmd.none)

    Tick _ -> 
      -- foldr to execute update, append to coins, replace bins
      let (updatedCoins, updatedBins) =
        List.foldr (\coin (coinList, bins) -> 
                       let (updatedCoin, updatedBins) = updateCoin (coin, bins) 
                       in (updatedCoin :: coinList, updatedBins))
                   ([], model.bins)
                   model.coins
      in ({ model | coins = updatedCoins, bins = updatedBins}, Cmd.none)

view : Model -> Html Msg
view model = 
  div []
    [ input
        [ placeholder "How many?"
        , let showString = if model.count > 0 then model.count |> toString else ""
          in value showString
        , onInput SetCount
        , disabled model.started
        , style [ ("height", "20px") ]
        , type' "number"
        , A.min "1"
        ]
        []

     , button
        [ onClick Go 
        , disabled model.started
        , style [ ("height", "20px") ]
        ]
        [ Html.text "GO!" ]

     , let coinForms = (List.map (drawCoin) model.coins)
       in collage Const.width Const.height (coinForms ++ drawGaltonBox) |> toHtml
    ]

subscriptions model =
    Sub.batch
        [ every (40*millisecond) Tick
        , every (200*millisecond) Drop
            -- drop = Signal.map (\t -> Drop (round t)) (every 200)
        ]

main =
  program 
      { init = init
      , view = view
      , update = update
      , subscriptions = subscriptions
      }
