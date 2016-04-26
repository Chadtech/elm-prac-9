module Thrusters where

import Types exposing (Ship)

weakPower : Float
weakPower = 0.1

mainPower : Float
mainPower = weakPower * 5

-- How much should the craft
-- rotate relative to 
-- how much its being thrust
-- in the cardinal directions
rtc : Float
rtc = 0.5

boost : Bool -> Float
boost b = if b then 10 else 1

deltaY : Ship -> Float
deltaY s = 
  let
    t  = s.thrusters
    c' = cos (degrees s.a)
    s' = sin (degrees s.a)
    tf = toFloat
  in
    ( (  mainPower * c' * tf t.main)       -- Main
    + (  weakPower * c' * tf t.leftBack)   -- left back
    + ( -weakPower * c' * tf t.leftFront)  -- left front
    + (  weakPower * c' * tf t.rightBack)  -- right back
    + ( -weakPower * c' * tf t.rightFront) -- right front
    + ( -weakPower * s' * tf t.leftSide)   -- left side
    + (  weakPower * s' * tf t.rightSide)  -- right side
    ) * boost t.boost



deltaX : Ship -> Float
deltaX s = 
  let
    t  = s.thrusters
    c' = cos (degrees s.a)
    s' = sin (degrees s.a)
    tf = toFloat
  in
    ( ( -mainPower * s' * tf t.main)       -- Main
    + ( -weakPower * s' * tf t.leftBack)   -- left back
    + (  weakPower * s' * tf t.leftFront)  -- left front
    + ( -weakPower * s' * tf t.rightBack)  -- right back
    + (  weakPower * s' * tf t.rightFront) -- right front
    + ( -weakPower * c' * tf t.leftSide)   -- left side
    + (  weakPower * c' * tf t.rightSide)  -- right side
    ) * boost t.boost



deltaAngular : Ship -> Float
deltaAngular s =
  let 
    t  = s.thrusters
    tf = toFloat
  in
    ( ( -weakPower * rtc * tf t.leftBack)
    + (  weakPower * rtc * tf t.leftFront)
    + (  weakPower * rtc * tf t.rightBack)
    + ( -weakPower * rtc * tf t.rightFront)
    ) * boost t.boost






