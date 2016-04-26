module Physics where

import Types exposing (Ship, frege)

checkYTop : Ship -> Float -> Float
checkYTop s y =
  if y > 250 then 
    checkYTop 
    { s | tileY = s.tileY + 1 }
    ( y - 500 )
  else y

checkYBottom : Ship -> Float -> Float
checkYBottom s y =
  if y < -250 then 
    checkYBottom 
    { s | tileY = s.tileY - 1 }
    ( y + 500 )
  else y

checkXRight : Ship -> Float -> Float
checkXRight s x =
  if x > 250 then 
    checkXRight 
    { s | tileX = s.tileX + 1 } 
    ( x - 500 )
  else x

checkXLeft : Ship -> Float -> Float
checkXLeft s x =
  if x < -250 then 
    checkXLeft 
    { s | tileX = s.tileX - 1 } 
    ( x + 500 )
  else x

physics : Float -> Ship -> Ship
physics dt s =
  { s
  | y = 
      checkYBottom s
      <|checkYTop s
      <|s.y + dt * s.vy
  , x = 
      checkXLeft s
      <|checkXRight s
      <|s.x + dt * s.vx
  , a = s.a + dt * s.va
  }