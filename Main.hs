module Main(main) where
    
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game


type Radius = Float 
type Position = (Float, Float)


width, height, offset :: Float
width = 300
height = 300
offset = 100

fps :: Int
fps = 600

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

-- Data describing the state of the pong game. 
data PongGame = Game
  { ballLoc :: (Float, Float)  
  , ballVel :: (Float, Float)   
  , player1 :: Float           
  , player2 :: Float
  , running :: Bool
  } deriving Show 

-- The starting state for the game of Pong.
initialState :: PongGame
initialState = Game
  { ballLoc = (0, 42)
  , ballVel = (150, 0)
  , player1 = 0
  , player2 = 0
  , running = True
  }

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

-- Update the ball position using its current velocity.
-- he number of seconds since last update
-- The initial game state
-- A new game state with an updated ball position
moveBall :: Float -> PongGame -> PongGame 
moveBall _ game@(Game {running=False}) = game
moveBall seconds game = game { ballLoc = (x', y') }
  where
    -- Old locations and velocities.
    (x, y) = ballLoc game
    (vx, vy) = ballVel game

    -- New locations.
    x' = x + vx * seconds
    y' = y + vy * seconds

-- | Update the game by moving the ball and bouncing off walls.
update :: Float -> PongGame -> PongGame
update seconds = paddleBounce . wallBounce . moveBall seconds

-- | Detect a collision with a paddle. Upon collisions,
-- change the velocity of the ball to bounce it off the paddle.
paddleBounce :: PongGame -> PongGame
paddleBounce game = game { ballVel = (vx', vy) }
  where
    radius = 10
    (vx, vy) = ballVel game
    loc1 = player1 game
    loc2 = player2 game
    vx' = if padleCollision (ballLoc game) (offset, loc1) || 
             padleCollision (ballLoc game) (-offset, loc2) 
          then -vx else vx

-- | Detect a collision with one of the side walls. Upon collisions,
-- update the velocity of the ball to bounce it off the wall.
wallBounce :: PongGame -> PongGame
wallBounce game = game { ballVel = (vx, vy') }
  where
    -- Radius. Use the same thing as in `render`.
    radius = 10

    -- The old velocities.
    (vx, vy) = ballVel game

    -- Update the velocity.
    vy' = if wallCollision (ballLoc game) radius then -vy else vy

          


wallCollision :: Position -> Radius -> Bool 
wallCollision (_, y) radius = topCollision || bottomCollision
  where
    topCollision    = y - radius <= -width / 2 
    bottomCollision = y + radius >=  width / 2



padleCollision :: Position -> Position -> Bool 
padleCollision (x, y) (px, py) = 
  (ceiling(x) == ceiling(px)) && ((y < py + 45) && (y > py - 45))
  
    

-- | Respond to key events.
handleKeys :: Event -> PongGame -> PongGame
handleKeys (EventKey (Char 's') _ _ _) game = game { ballLoc = (0, 0) }
handleKeys (EventKey (Char 'p') Down _ _) game = 
  game { running = runState}
    where
      runState = not $ running game
handleKeys _ game = game

                    
main :: IO ()
main = play window background fps initialState render handleKeys update
 