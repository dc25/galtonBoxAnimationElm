import Ball exposing (Model, init, update, view, tick)
import StartApp exposing (..)
import Effects exposing (Effects)
import Html exposing (Html)

app : StartApp.App Model
app = StartApp.start 
  { init = (init, Effects.none)
  , update = update
  , view = view
  , inputs = [ tick ]
  }

main : Signal Html
main = app.html
