module Frame where

import Color            exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import List             exposing (..)
import Types            exposing (Ship)
import DrawLander       exposing (..)
import World            exposing (world)
import String
import HUD              exposing (..)
import Minimap          exposing (minimap)
import Source           exposing (src)

barBlue : Color
barBlue = rgb 5 3 21

bar : Float -> Float -> (Float, Float) -> Form
bar w h cor =
  rect w h |> filled barBlue |> move cor

blinders : (Int, Int) -> Form
blinders (w,h) =
  let
    vBarHeight = (toFloat (h - 501))/2 + 1
    hBarWidth  = (toFloat (w - 501))/2 + 1
    hBarHeight = (toFloat h) - (vBarHeight * 2) + 2
    w'         =  toFloat w
    h'         = (toFloat h)/2
  in
    collage w h
    [ bar w'        vBarHeight (0, (h' - vBarHeight/2))
    , bar w'        vBarHeight (0, (-h' + vBarHeight/2)) 
    , bar hBarWidth hBarHeight (-(w' - 500)/2 - 15, 0)
    , bar hBarWidth hBarHeight ((w'- 500)/2 + 15, 0)
    ]
    |>toForm

frame : (Int, Int) -> Ship -> Form -> Element
frame (w,h) s world =
  collage w h
  [ collage w  h
    [ rotate (degrees -s.a) world
    , src "scope"
      |>image 501 501
      |>toForm
    , blinders (w,h)
    , hud s
    , move (-450, 78) keys
    , minimap s
    , drawLander s
    ]
    |>toForm
  ]

