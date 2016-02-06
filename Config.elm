module Config where

import Time exposing (Time)

hscale : Float
hscale = 12

vscale : Float
vscale = hscale * 2

dropInterval : Time
dropInterval = 200

stepInterval : Time
stepInterval = 150

width : Int
width = 1000

height : Int
height = 1000

headRoom : Int
headRoom = 50

maxDrop : Float
maxDrop = 600.0

levelCount : Int
levelCount = 9

ballDiameter : Float
ballDiameter = hscale/ 2.0

