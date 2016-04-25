module Physics where

import Types exposing (Ship, frege)

checkYTop : Float -> Float
checkYTop y =
  if y > 250 then checkYTop (y - 500)
  else y

checkYBottom : Float -> Float
checkYBottom y =
  if y < -250 then checkYBottom (y + 500)
  else y

checkXRight : Float -> Float
checkXRight x =
  if x > 250 then checkXRight (x - 500)
  else x

checkXLeft : Float -> Float
checkXLeft x =
  if x < -250 then checkXLeft (x + 500)
  else x

physics : Float -> Ship -> Ship
physics dt frege =
  { frege
  | y = 
      checkYBottom
      <|checkYTop 
      <|frege.y + dt * frege.vy
  , x = 
      checkXLeft
      <|checkXRight
      <|frege.x + dt * frege.vx
  , a = frege.a + dt * frege.va
  }