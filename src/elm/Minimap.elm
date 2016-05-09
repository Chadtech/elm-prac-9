module Minimap where

import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Html             exposing (..)
import Html.Attributes  exposing (style)
import List             exposing (..)
import Types            exposing (Ship)
import DrawLander       exposing (..)
import World            exposing (world)
import String
import List             exposing (unzip, map)
import Source           exposing (src)
import Csscolors        exposing (..)

(.) = (,)

tf = toFloat

minimap : Ship -> Form
minimap s =
  let
    x = (tf s.tileX * 500  + 250  + s.x) / 110
    y = (tf s.tileY * 500  + 250  + s.y) / 110
  in
    div 
    [ style
      [ "background-color" . backgroundColor'
      , "border-style"     . "solid" 
      , "border-width"     . "2px"
      , "border-color"     . pointColor
      , "padding"          . "1em"
      , "width"            . "220px"
      , "height"           . "220px"
      ] 
    ] 
    [ collage 220 220
      [ src "real-stars"
        |>image 160 125
        |>toForm
        |>alpha 0.05
        |>rotate (degrees 0)
        |>move (-50, 0)
      , src "lander"
        |>image 2 2
        |>toForm
        |>move (x - 110, y - 110)
      , src "stars-aseprite-2"
        |>image 10 10
        |>toForm
        |>move (-79.6, 82)
      ]
      |>fromElement
    ]
    |>toElement 0 0
    |>toForm
    |>move (260, 298)
