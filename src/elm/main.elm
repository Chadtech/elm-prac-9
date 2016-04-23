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
import Types            exposing (..)
import KeyCodes
import Task
import Effects          exposing (..)
import Set              exposing (Set)

-- The landers name is Reasey
frege : Ship
frege = 
  { x            = 0
  , y            = 0
  , a            = 15
  , vx           = 0
  , vy           = 0
  , va           = -0.1
  , thrusters    =
    { leftFront  = 0
    , leftSide   = 0
    , leftBack   = 0
    , main       = 0
    , rightFront = 0
    , rightSide  = 0
    , rightBack  = 0
    }
  }


keyPressed : Int -> Set Int -> Int
keyPressed key keys =
  if Set.member key keys then
    1
  else
    0


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
    }
  }


thrust : Ship -> Ship
thrust frege =
  { frege
  | vy = frege.vy + 0.5 * (deltaY       frege)
  , vx = frege.vx + 0.5 * (deltaX       frege)
  , va = frege.va + 0.5 * (deltaAngular frege)
  }

gravity : Float -> Ship -> Ship
gravity dt frege =
  { frege
  | vy = frege.vy - dt/50
  , vx = frege.vx - dt/94
  }

floatModulo: Float -> Int -> Float
floatModulo n m =
  let
    n' = 
      round n

    modulod =
      n' % m
  in
    (-) (toFloat modulod)
    <|  (toFloat n') - n


physics : Float -> Ship -> Ship
physics dt frege =
  let 
    y' = 
      let 
        y'' = 
          frege.y + dt * frege.vy
      in    
        --floatModulo y''  250
        if y'' > 250 then
          y'' - 500
        else
          if y'' < -250 then
            y'' + 500
          else
            y''

    x' = 
      let
        x'' = 
          frege.x + dt * frege.vx
      in
        if x'' > 250 then
          x'' - 500
        else
          if x'' < -250 then
            x'' + 500
          else
            x''

  in
    { frege
    | y = y'
    , x = x'
    , a = frege.a + dt * frege.va
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


