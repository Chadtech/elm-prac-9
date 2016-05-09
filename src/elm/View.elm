module View where

import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Html             exposing (..)
import Html.Events      exposing (on, targetValue)
import List             exposing (..)
import Types            exposing (Ship)
import World            exposing (world)
import Frame            exposing (..)
import List             exposing (take, drop, head, tail)
import Maybe            exposing (withDefault)
import String           exposing (slice)
import Source           exposing (src)

wd  = withDefault
wd' = wd "wd"

tile : String -> Form
tile t =
  toForm <| image 501 501 <| src t

stars : Form
stars = tile "stars-aseprite-6"

planet : Form
planet = 
  collage 501 501
  [ tile "stars-aseprite-6"
  , tile "stars-aseprite-2"
  ]
  |>toForm

tileSelector : String -> Form
tileSelector str =
  if str == "p" then planet
  else stars

space : (Int, Int) -> Ship  -> Form
space (w,h) s =
  let
    (top, bottom)  = 
      let 
        x = s.tileX
        y = 45 - s.tileY

        rows' =
          world
          |>drop y
          |>take 2
          |>map (slice x (x + 2))
      in
        (,) (wd' (head rows'))
        <|   wd' (head (wd [] (tail rows')))
  in
    collage w h
    [ move (-250,  250 ) 
      <|tileSelector 
      <|slice 0 1 top
    , move ( 250,  250 ) 
      <|tileSelector 
      <|slice 1 2 top
    , move ( 250, -250 ) 
      <|tileSelector 
      <|slice 1 2 bottom
    , move (-250, -250 )
      <|tileSelector 
      <|slice 0 1 bottom
    ]
    |>toForm

reposition : (Int, Int) -> Ship -> Form -> Form
reposition (w,h) s world = 
  collage w h [ move (-s.x,-s.y) world ]
  |>toForm

backdrop : (Int, Int) -> Ship -> Form -> Form
backdrop (w,h) s world =
  let
    x = 
      ((toFloat s.tileX) * 500) + 50 + s.x
    y = 
      ((toFloat s.tileY) * 500) + 250 + s.y
    pos = 
      (-x * 0.005, -y * 0.005)
  in
    collage 1000 1000
    [ src "real-stars"
      |>image 320 250
      |>toForm
      |>alpha 0.05
      |>move pos
    , world
    ]
    |>toForm

view : (Int, Int) -> Ship -> Element
view d s =
  let
    -- World Size
    ws = (1000,1000)
  in
    space        ws s
    |>reposition ws s
    |>backdrop   ws s
    |>frame      d  s





