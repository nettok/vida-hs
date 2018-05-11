module Main where

import Control.Monad (mapM_)

import Graphics.Gloss.Interface.IO.Game (Display(..), white, playIO)

import Game
import Life (makeEmptySquareUGrid, runInstructionUGrid, createGliderInstructions)

main :: IO ()
main = do
  let window = InWindow "Vida" (width, height) (100, 50)
  let background = white
  let fps = 5
  let gridSize = width `div` (round cellRadius * 2)

  initialGrid <- Life.makeEmptySquareUGrid gridSize
  mapM_ (Life.runInstructionUGrid initialGrid) (Life.createGliderInstructions 1 1)

  let initialState = World { wGrid = initialGrid, wPaused = False }

  playIO window background fps initialState render handleEvents update
