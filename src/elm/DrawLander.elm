module DrawLander where

import Color            exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import List             exposing (..)
import Root             exposing (root)
import Types            exposing (Ship)


src : String -> String
src str =
  root ++ str ++ ".png"

imager : Int -> Int -> String -> Form
imager w h str =
  toForm <| image w h str

drawLander : Ship -> Form
drawLander s =
  let
    t       = s.thrusters
    sides   = \i -> toForm <|image 2 9 i
    strafer = \i -> toForm <|image 8 3 i

    lander = 
      [ imager 47 48 
        <|"./darkened-lander.png" 
      ]

    mainThruster = 
      if t.main >= 1 then
        [ move (0, -30) 
          <|imager 11 30 
          <|"./blast_main-weak.png"
        ]
      else []

    leftFront =
      if t.leftFront >= 1 then
        [ move (-19, 9) 
          <|sides ("./blast_yaw-weak.png")
        ]
      else []

    leftBack =
      if t.leftBack >= 1 then
        [ move (-19, -9) 
          <|scale -1
          <|sides ("./blast_yaw_f-weak.png")
        ]
      else []

    leftSide = 
      if t.leftSide >= 1 then
        [ move (25, -1)
          <|rotate (degrees 180)
          <|strafer ("./blast_strafe-weak.png")
        ]
      else []

    rightSide =
      if t.rightSide >= 1 then
        [ move (-25, -1)
          <|strafer ("./blast_strafe-weak.png")
        ]
      else []

    rightFront =
      if t.rightFront >= 1 then
        [ move (19, 9) 
          <|rotate (degrees 180) 
          <|scale -1  
          <|sides ("./blast_yaw_f-weak.png")
        ]
      else []

    rightBack =
      if t.rightBack >= 1 then
        [ move (19, -9)
          <|rotate (degrees 180)
          <|sides ("./blast_yaw-weak.png")
        ]
      else []

  in
    toForm
    <|collage 138 138
      <|List.foldr append []
        [ rightSide
        , leftSide
        , rightBack
        , leftBack
        , rightFront
        , leftFront
        , mainThruster
        , lander
        ]



