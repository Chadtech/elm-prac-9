module Shipupdate where

import List             exposing (..)
import Thrusters        exposing (deltaY, deltaX, deltaAngular)
import Types            exposing (Ship, frege)
import KeyCodes
import Set              exposing (Set, member)

keyPressed : Int -> Set Int -> Int
keyPressed key keys =
  if Set.member key keys then 1 else 0

thrust : Ship -> Ship
thrust s =
  let
    wc = (1704 / (s.weight))
  in
    if s.fuel > 0 then
      { s
      | vy = s.vy + (wc * (deltaY       s))
      , vx = s.vx + (wc * (deltaX       s))
      , va = s.va + (wc * (deltaAngular s))
      }
    else 
      { s | fuel = 0 }

weight : Ship -> Ship
weight s =
  { s
  | weight = 
      852 + (s.fuel * 0.6) + (s.oxygen * 3)
  }

fuel : Ship -> Ship
fuel s =
  { s 
  | fuel =
    (-) s.fuel
      <|(*) 0.1
      <|toFloat
      <|foldr (+) 0
      [ 4 * s.thrusters.main 
      , 7 * s.thrusters.leftBack
      , 7 * s.thrusters.leftFront
      , 7 * s.thrusters.rightBack
      , 7 * s.thrusters.rightFront
      , 7 * s.thrusters.leftSide
      , 7 * s.thrusters.rightSide
      ]
  }

oxygen : Float -> Ship -> Ship
oxygen dt s =
  if s.oxygen > 0 then
    { s | oxygen = s.oxygen - (dt / 150) }
  else 
    { s | oxygen = 0 }

setThrusters : Set Int -> Ship -> Ship
setThrusters keys s =
  { s
  | thrusters = 
    { leftFront  = keyPressed KeyCodes.c     keys
    , leftSide   = keyPressed KeyCodes.s     keys
    , leftBack   = keyPressed KeyCodes.e     keys
    , main       = keyPressed KeyCodes.space keys
    , rightFront = keyPressed KeyCodes.n     keys
    , rightSide  = keyPressed KeyCodes.k     keys
    , rightBack  = keyPressed KeyCodes.u     keys
    , boost      = Set.member KeyCodes.shift keys
    }
  }

gravity : Float -> Ship -> Ship
gravity dt s =
  { s
  | vy = s.vy - dt/50
  , vx = s.vx - dt/94
  }