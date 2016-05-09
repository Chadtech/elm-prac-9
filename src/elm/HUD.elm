module HUD where

import Graphics.Collage exposing (collage, toForm, Form, move)
import Graphics.Element exposing (..)
import Html             exposing (..)
import Html.Attributes  exposing (style, class)
import Types            exposing (Ship)
import String
import List             exposing (unzip, map)
import String           exposing (slice)
import Csscolors        exposing (..)
import Source           exposing (src)

(.) = (,)

cut : Int -> String -> String
cut limit str =
  if (String.length str) > limit then
    slice 0 limit str 
  else str

-- Number Format
nf : Float -> String
nf f = cut 8 (toString f)

-- Position Format
pf : Int -> Float -> String
pf t p =
  nf (((p + 250) + (toFloat t) * 500) / 10)

round' : Float -> Float
round' f =
  (toFloat (round (f * 10))) / 10

readOut : Ship -> (List String, List String)
readOut s =
  unzip
  [ "--------"  . "--------"
  , "STATUS"    . "NOMINAL"
  , "--------"  . "--------"
  , "ang vel "  . nf -(s.va * 10)
  , "velocity"  . nf ((((s.vx ^ 2) + (s.vy ^ 2)) ^ 0.5)/10)
  , "position"  . "--------"
  , ": angle"   . nf -(s.a / 0.9)
  , ": x"       . pf s.tileX s.x
  , ": y"       . pf s.tileY s.y
  , "--------"  . "--------"
  , "FUEL"      . ((cut 6 (toString (round' s.fuel)))   ++ "l"  )
  , "OXYGEN"    . ((cut 6 (toString (round  s.oxygen))) ++ "l"  )
  , "WEIGHT"    . ((cut 6 (toString (round  s.weight))) ++ " yH")
  ]

point : String -> Html
point str =
  p 
  [ class "point"
  , style [ "width" . "110px" ] 
  ] 
  [ text str ]

hud : Ship -> Form
hud s =
  let
    (keys, values) = readOut s

    colStyle = 
      [ style [ "float" . "left" ] ]
  in
    div 
    [ style
      [ "background-color" . backgroundColor
      , "border-style"     . "solid" 
      , "border-width"     . "2px"
      , "border-color"     . pointColor
      , "padding"          . "1em"
      , "width"            . "220px"
      , "height"           . "290px"
      ] 
    ] 
    [ p [class "point"] [text "READ OUT"]
    , div colStyle <| map point keys
    , div colStyle <| map point values
    ]
    |>toElement 0 0
    |>toForm
    |>move (260, 45)

keys : Form
keys =
  div
  [ style
    [ "background-color" . backgroundColor
    , "border-style"     . "solid" 
    , "border-width"     . "2px"
    , "border-color"     . pointColor
    , "padding"          . "1em"
    , "width"            . "156px"
    , "height"           . "131px"
    ] 
  ]
  [ src "key_diagram"
    |>image 156 131
    |>fromElement
  ]
  |>toElement 0 0
  |>toForm
