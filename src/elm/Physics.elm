module Physics where

import Types exposing (Ship, frege)

rollYTop : Float -> Float
rollYTop y =
  if y > 250 then 
    rollYTop (y - 500)
  else y

rollYBottom : Float -> Float
rollYBottom y =
  if y < -250 then 
    rollYBottom (y + 500)
  else y

rollXRight : Float -> Float
rollXRight x =
  if x > 250 then 
    rollXRight (x - 500)
  else x

rollXLeft : Float -> Float
rollXLeft x =
  if x < -250 then 
    rollXLeft (x + 500)
  else x

rollAngleC : Float -> Float
rollAngleC a =
  if a > 180 then
    rollAngleC (a - 3600)
  else a

rollAngleCC : Float -> Float
rollAngleCC a =
  if a < -180 then
    rollAngleCC (a + 360)
  else a

physics : Float -> Ship -> Ship
physics dt s =
  let 
    y' = s.y + dt * s.vy
    x' = s.x + dt * s.vx

    ym =
      rollYTop y'
      |>rollYBottom

    xm =
      rollXRight x'
      |>rollXLeft

    dyt = 
      (round (y' - ym)) // 500 
    dxt = 
      (round (x' - xm)) // 500
  
  in
    { s
    | y     = ym
    , x     = xm
    , tileY = s.tileY + dyt
    , tileX = s.tileX + dxt
    , a     = 
        s.a + dt * s.va
        |>rollAngleC
        |>rollAngleCC

    }






