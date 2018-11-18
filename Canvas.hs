module Canvas where
    
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Constants

window :: Display
window = InWindow "Nice Window" (200, 200) (10, 10)

background :: Color
background = white

drawing :: Picture
drawing = pictures [ball, walls,
                    mkPaddle rose 120 (-20),
                    mkPaddle orange (-120) 40]

  where
    --  The pong ball.
    ball = translate (-10) 40 $ color ballColor $ circleSolid 10
    ballColor = dark red

    --  The bottom and top walls.
    wall :: Float -> Picture
    wall offset =
      translate 0 offset $
        color wallColor $
          rectangleSolid 270 10

    wallColor = greyN 0.5
    walls = pictures [wall 150, wall (-150)]

    --  Make a paddle of a given border and vertical offset.
    mkPaddle :: Color -> Float -> Float -> Picture
    mkPaddle col x y = pictures
      [ translate x y $ color col $ rectangleSolid 26 86
      , translate x y $ color paddleColor $ rectangleSolid 20 80
      ]

    paddleColor = light (light blue)

-- The game state to render -> A picture of this game state.
render :: PongGame -> Picture   
render game =
  pictures [ball, walls,
            mkPaddle rose 120 $ player1 game,
            mkPaddle orange (-120) $ player2 game]
  where
    --  The pong ball.
    ball = uncurry translate (ballLoc game) $ color ballColor $ circleSolid 10
    ballColor = dark red

--  The bottom and top walls.
wall :: Float -> Picture
wall offset =
  translate 0 offset $
    color wallColor $
      rectangleSolid 270 10

wallColor = greyN 0.5
walls = pictures [wall 150, wall (-150)]

--  Make a paddle of a given border and vertical offset.
mkPaddle :: Color -> Float -> Float -> Picture
mkPaddle col x y = pictures
  [ translate x y $ color col $ rectangleSolid 26 86
  , translate x y $ color paddleColor $ rectangleSolid 20 80
  ]

paddleColor = light (light blue)