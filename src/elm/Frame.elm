module Frame where

import Color            exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Html             exposing (..)
import Html.Attributes  exposing (..)
import Html.Events      exposing (on, targetValue)
import List             exposing (..)
import Root             exposing (..)
import Types            exposing (Ship)
import DrawLander       exposing (..)
import World            exposing (world)
import String
import HUD              exposing (..)


bar : Float -> Float -> (Float, Float) -> Form
bar w h cor =
  rect w h
  |>filled (rgb 11 07 43)
  |>move cor

blinders : (Int, Int) -> Form
blinders (w,h) =
  let
    vBarHeight = (toFloat (h - 501))/2 + 1
    hBarWidth  = (toFloat (w - 501))/2 + 1
    hBarHeight = (toFloat h) - (vBarHeight * 2) + 2
    w'         =  toFloat w
    h'         = (toFloat h)/2
  in
    toForm
    <|collage w h
      [ bar w'        vBarHeight (0, (h' - vBarHeight/2))
      , bar w'        vBarHeight (0, (-h' + vBarHeight/2)) 
      , bar hBarWidth hBarHeight (-(w' - 500)/2 - 15, 0)
      , bar hBarWidth hBarHeight ((w'- 500)/2 + 15, 0)
      ]


frame : (Int, Int) -> Ship -> Form -> Element
frame (w,h) s world =
  collage w h
  [ rotate (degrees -s.a) world
  , toForm 
    <|image 501 501 
    <|"./scope.png"
  , blinders (w,h)
  , hud s
  , drawLander s
  ]