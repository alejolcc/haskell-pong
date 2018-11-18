module Main(main) where
    
import Constants
import GameState
import Canvas
import Graphics.Gloss.Interface.Pure.Game

main :: IO ()
main = play window background fps initialState render handleKeys update
 