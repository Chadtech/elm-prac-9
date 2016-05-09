module DrawLander where

import Color            exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import List             exposing (..)
import Types            exposing (Ship)
import Source           exposing (src)

imager : Int -> Int -> String -> Form
imager w h str =
  toForm <| image w h str

drawRotator : String -> Form
drawRotator str =
  toForm <| image 2 9 str

drawStrafer : String -> Form
drawStrafer str = 
  toForm <| image 8 3 str

mainThruster : Int -> Bool -> List Form
mainThruster firing boost =
  let
    blast =
      if boost then
        src "blast_main-1"
      else
        src "blast_main-weak"
  in
    if firing >= 1 then
    [ image 11 30 blast
      |>toForm
      |>move (0, -30)
    ]
    else []

leftFront : Int -> Bool -> List Form
leftFront firing boost =
  let
    blast = 
      if boost then 
        src "blast_yaw"
      else 
        src "blast_yaw-weak"
  in
    if firing >= 1 then
    [ drawRotator blast
      |> move (-19, 9)
    ]
    else []

leftBack : Int -> Bool -> List Form
leftBack firing boost =
  let
    blast = 
      if boost then 
        src "blast_yaw_f"
      else 
        src "blast_yaw_f-weak"
  in
    if firing >= 1 then
    [ drawRotator blast
      |>scale -1
      |>move (-19, -9)
    ]
    else []

leftSide : Int -> Bool -> List Form
leftSide firing boost =
  let
    blast =
      if boost then
        src "blast_strafe"
      else
        src "blast_strafe-weak"
  in
    if firing >= 1 then
    [ drawStrafer blast
      |>rotate (degrees 180)
      |>move (25, -1)
    ]
    else []

rightSide : Int -> Bool -> List Form
rightSide firing boost =
  let
    blast =
      if boost then
        src "blast_strafe"
      else
        src "blast_strafe-weak"
  in
    if firing >= 1 then
    [ drawStrafer blast
      |>move (-23, -1)
    ]
    else []

rightFront : Int -> Bool -> List Form
rightFront firing boost =
  let
    blast =
      if boost then
        src "blast_yaw_f"
      else
        src "blast_yaw_f-weak"
  in
    if firing >= 1 then
    [ drawRotator blast
      |>scale -1
      |>rotate (degrees 180)
      |> move (19, 9)
    ]
    else []

rightBack : Int -> Bool -> List Form
rightBack firing boost =
  let
    blast =
      if boost then
        src "blast_yaw"
      else 
        src "blast_yaw_f-weak"
  in
    if firing >= 1 then
    [ drawRotator blast
      |>rotate (degrees 180)
      |>move (19, -9) 
    ]
    else []

drawLander : Ship -> Form
drawLander s =
  let
    t = s.thrusters

    lander =
      if s.fuel > 0 then
        [ rightSide    t.rightSide  t.boost
        , leftSide     t.leftSide   t.boost
        , rightBack    t.rightBack  t.boost
        , leftBack     t.leftBack   t.boost
        , rightFront   t.rightFront t.boost
        , leftFront    t.leftFront  t.boost
        , mainThruster t.main       t.boost
        , [ imager 47 48 (src "lander") ]
        ]
      else
        [ [ imager 47 48 (src "lander") ] ]
  in
    toForm
    <|collage 138 138
    <|foldr append [] lander



