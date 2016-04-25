import Color            exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard
import Time             exposing (..)
import Window
import Set
import List             exposing (..)
import Random
import Thrusters        exposing (deltaY, deltaX, deltaAngular)
import View             exposing (view)
import Physics          exposing (physics)
import Types            exposing (Ship, frege)
import KeyCodes
import Task
import Effects          exposing (..)
import Set              exposing (Set)

keyPressed : Int -> Set Int -> Int
keyPressed key keys =
  if Set.member key keys then 1 else 0

setThrusters : Set Int -> Ship -> Ship
setThrusters keys s =
  -- Buttons correspond to direction of thrust 
  -- not thruster position on craft
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

thrust : Ship -> Ship
thrust frege =
  { frege
  | vy = frege.vy + (deltaY       frege)
  , vx = frege.vx + (deltaX       frege)
  , va = frege.va + (deltaAngular frege)
  }

gravity : Float -> Ship -> Ship
gravity dt frege =
  { frege
  | vy = frege.vy - dt/50
  , vx = frege.vx - dt/94
  }

update : (Float, Set Int) -> Ship -> Ship
update (dt, keys) frege =
  frege
    |>setThrusters keys
    --|> gravity dt
    |>thrust
    |>physics dt

main : Signal Element
main =
  Signal.map2 view Window.dimensions
  <|Signal.foldp update frege input

input : Signal (Float, Set Int)
input =
  let 
    delta = Signal.map (\t -> t/120) (fps 30)
  in
    Signal.sampleOn delta 
    <|Signal.map2 (,) delta Keyboard.keysDown


