module Config where

import Time exposing (Time)

hscale : Float
hscale = 8

vscale : Float
vscale = hscale * 2

dropInterval : Time
dropInterval = 300

stepInterval : Time
stepInterval = 180

width : Int
width = 1000

height : Int
height = 1000

headRoom : Int
headRoom = 50

maxDrop : Float
maxDrop = 600.0

levelCount : Int
levelCount = 12

ballDiameter : Float
ballDiameter = hscale/ 2.0

