module Thrusters where

import Types exposing (Ship)

tf : Int -> Float
tf = toFloat

weakPower : Float
weakPower = 0.128

mainPower : Float
mainPower = weakPower * 7

-- How much should the craft
-- rotate relative to 
-- how much its being thrust
-- in the cardinal directions
rtc : Float
rtc = 0.5

boost : Bool -> Float
boost b = if b then 5 else 1

deltaY : Ship -> Float
deltaY s = 
  let
    t  = s.thrusters
    c' = cos (degrees s.a)
    s' = sin (degrees s.a)
    b  = boost t.boost
  in
    (*) b
    <|List.foldr (+) 0
      [  mainPower * c' * tf t.main
      ,  weakPower * c' * tf t.leftBack  
      , -weakPower * c' * tf t.leftFront 
      ,  weakPower * c' * tf t.rightBack 
      , -weakPower * c' * tf t.rightFront
      , -weakPower * s' * tf t.leftSide   
      ,  weakPower * s' * tf t.rightSide  
      ]

deltaX : Ship -> Float
deltaX s = 
  let
    t  = s.thrusters
    c' = cos (degrees s.a)
    s' = sin (degrees s.a)
    b  = boost t.boost
  in
    (*) b 
    <|List.foldr (+) 0   
    [ -mainPower * s' * tf t.main
    , -weakPower * s' * tf t.leftBack
    ,  weakPower * s' * tf t.leftFront
    , -weakPower * s' * tf t.rightBack
    ,  weakPower * s' * tf t.rightFront
    , -weakPower * c' * tf t.leftSide
    ,  weakPower * c' * tf t.rightSide
    ]


deltaAngular : Ship -> Float
deltaAngular s =
  let 
    t  = s.thrusters
  in
    ( ( -weakPower * rtc * tf t.leftBack)
    + (  weakPower * rtc * tf t.leftFront)
    + (  weakPower * rtc * tf t.rightBack)
    + ( -weakPower * rtc * tf t.rightFront)
    ) * boost t.boost






