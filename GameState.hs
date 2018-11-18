module GameState where
    
import Graphics.Gloss.Interface.Pure.Game

import Constants

data Movement = UpMove | DownMove | Stay deriving Show
type Radius = Float 
type Position = (Float, Float)


-- Data describing the state of the pong game. 
data PongGame = Game
  { ballLoc :: (Float, Float)  
  , ballVel :: (Float, Float)   
  , player1 :: Float           
  , player2 :: Float
  , running :: Bool
  , player1mov :: Movement
  , player2mov :: Movement
  } deriving Show 

-- The starting state for the game of Pong.
initialState :: PongGame
initialState = Game
  { ballLoc = (0, 42)
  , ballVel = (150, 0)
  , player1 = 0
  , player2 = 0
  , running = True
  , player1mov = Stay
  , player2mov = Stay
  }

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





movePaddle :: PongGame -> PongGame
movePaddle game = game {player1 = newPos1, player2 = newPos2}
    where 
      pos1 = player1 game
      pos2 = player2 game
      p1mov = player1mov game
      p2mov = player2mov game
      newPos1 = updatePos pos1 p1mov
      newPos2 = updatePos pos2 p2mov

updatePos :: Float -> Movement -> Float
updatePos pos UpMove = pos + 1
updatePos pos DownMove = pos - 1
updatePos pos Stay = pos




-- | Update the game by moving the ball and bouncing off walls.
update :: Float -> PongGame -> PongGame
update seconds = paddleBounce . wallBounce . movePaddle . moveBall seconds

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
-- Restart and pause
handleKeys (EventKey (Char 's') _ _ _) game = game { ballLoc = (0, 0) }
handleKeys (EventKey (Char 'p') Down _ _) game = 
  game { running = runState}
    where
      runState = not $ running game
-- Movements Player1
handleKeys (EventKey (SpecialKey KeyUp) Down _ _) game = game {player1mov = UpMove}
handleKeys (EventKey (SpecialKey KeyUp) Up _ _) game = game {player1mov = Stay}
handleKeys (EventKey (SpecialKey KeyDown) Down _ _) game = game {player1mov = DownMove}
handleKeys (EventKey (SpecialKey KeyDown) Up _ _) game = game {player1mov = Stay}

-- Movements Player2
handleKeys (EventKey (Char 'q') Down _ _) game = game {player2mov = UpMove}
handleKeys (EventKey (Char 'q') Up _ _) game = game {player2mov = Stay}
handleKeys (EventKey (Char 'a') Down _ _) game = game {player2mov = DownMove}
handleKeys (EventKey (Char 'a') Up _ _) game = game {player2mov = Stay}

handleKeys _ game = game
