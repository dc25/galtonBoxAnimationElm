module Config where

import Time exposing (Time)

hscale : Float
hscale = 10

vscale : Float
vscale = hscale * 2

topMargin : Int
topMargin = 30

bottomMargin : Int
bottomMargin = 30

levelCount : Int
levelCount = 12

dropCount : Int
dropCount = 90

ballDiameter : Float
ballDiameter = hscale/ 2.0
