module HUD where

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

text = Html.text

(:::) = (,)

cut : Int -> String -> String
cut limit str =
  if (String.length str) > limit then
    String.slice 0 limit str 
  else str

-- Number Format
nf : Float -> String
nf f = cut 6 (toString f)

readOut : Ship -> List (String, String)
readOut s =
 [ "Ang Vel : " ::: (nf s.va)
 , "Angle : "   ::: (nf (s.a / 90))
 ]

hud : Ship -> Form
hud s =
  move (260, 250)
  <|toForm
  <|toElement 0 0
  <|div 
    [ style
      [ "background-color" ::: "#030907" 
      , "padding"          ::: "1em"
      , "width"            ::: "400px"
      , "height"           ::: "500px"
      ] 
    ] 
    [ p [ class "point" ] [ text "STATUS NOMINAL" ]
    , p 
      [ class "point" ] 
      [ text 
        <|"Vel Ang : " ++ (cut 6 (toString s.va)) ]
    , p 
      [ class "point" ] 
      [ text 
        <|"Angle : " ++ (cut 6 (toString (s.a / 90))) ]
    ]
