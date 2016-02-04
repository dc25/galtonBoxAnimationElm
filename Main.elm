import Balls exposing (Model, init, update, view, tick, drop)
import StartApp exposing (..)
import Effects exposing (Effects)
import Html exposing (Html)

app : StartApp.App Model
app = StartApp.start 
  { init = (init, Effects.none)
  , update = update
  , view = view
  , inputs = [ drop, tick ]
  }

main : Signal Html
main = app.html
