module Config where

import Time exposing (Time)

hscale : Float
hscale = 12

vscale : Float
vscale = hscale * 2

dropInterval : Time
dropInterval = 200

stepInterval : Time
stepInterval = 50

topMargin : Int
topMargin = 30

bottomMargin : Int
bottomMargin = 30

levelCount : Int
levelCount = 12

dropCount : Int
dropCount = 150

ballDiameter : Float
ballDiameter = hscale/ 2.0

