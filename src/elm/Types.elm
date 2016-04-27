module Types where

type alias Ship =
  { x:            Float
  , y:            Float
  , a:            Float

  , vx:           Float
  , vy:           Float
  , va:           Float

  , tileX:        Int
  , tileY:        Int

  , thrusters: 
    { leftFront:  Int
    , leftSide:   Int
    , leftBack:   Int
    , main:       Int
    , rightFront: Int
    , rightSide:  Int
    , rightBack:  Int
    , boost:      Bool
    }
  }


frege : Ship
frege = 
  { x            = 0
  , y            = 0
  , a            = 0

  , vx           = 0
  , vy           = 0
  , va           = -0.1

  , tileX        = 0
  , tileY        = 0

  , thrusters    =
    { leftFront  = 0
    , leftSide   = 0
    , leftBack   = 0
    , main       = 0
    , rightFront = 0
    , rightSide  = 0
    , rightBack  = 0
    , boost      = False
    }
  }