import Graphics.Element exposing (..)
import Keyboard         exposing (..)
import Time             exposing (..)
import Window
import View             exposing (view)
import Physics          exposing (physics)
import Types            exposing (Ship, frege)
import Shipupdate       exposing (..)
import Set              exposing (Set)
import Signal           exposing (..)


update : (Float, Set Int) -> Ship -> Ship
update (dt, keys) s =
  s
  |>setThrusters keys
  --|> gravity dt
  |>weight
  |>thrust
  |>fuel
  |>oxygen dt
  |>physics dt

main : Signal Element
main =
  map2 view Window.dimensions
  <|foldp update frege input

input : Signal (Float, Set Int)
input =
  let 
    d = map (\t -> t/120) (fps 30)
  in
    map2 (,) d keysDown
    |>sampleOn d 


