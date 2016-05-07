module View where

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
import Frame            exposing (..)

text = Html.text

stars : Form
stars = 
  toForm 
  <|image 501 501 
  <|"./stars-aseprite-3.png"

planet : Form
planet = 
  toForm
  <|collage 501 501
    [ toForm 
      <|image 501 501 
      <|"./stars-aseprite-3.png"
    , toForm 
      <|image 501 501 
      <|"./stars-aseprite-2.png"
    ]

setUp : Ship -> (Int, Int) -> Form
setUp s (w,h) =
  toForm
  <|collage w h
    [ move (-250,  250) planet
    , move ( 250,  250) stars
    , move ( 250, -250) stars
    , move (-250, -250) stars
    ]

reposition : (Int, Int) -> Ship -> Form -> Form
reposition (w,h) s world = 
  toForm
  <|collage w h
    [ move (-s.x,-s.y) world ]


view : (Int, Int) -> Ship -> Element
view d s =
  setUp s (1000,1000)
  |>reposition (1000,1000) s
  |>frame d s





