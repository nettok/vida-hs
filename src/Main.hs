module Main where

import Control.Monad

import Graphics.Gloss.Interface.IO.Game

import Life

type World = Life.UGrid IO Bool

main :: IO ()
main = do
  let window = InWindow "Vida" (800, 800) (100, 50)
  let background = white
  let fps = 1
  initialState <- Life.makeEmptySquareUGrid 20
  mapM_ (Life.runInstructionUGrid initialState) (Life.createGliderInstructions 10 10)
  playIO window background fps initialState render handleEvents update

render :: World -> IO Picture
render w = Pictures <$> Life.ifoldUGrid renderCell [] w
  where
    renderCell :: Int -> Int -> Life.NeighbourRows IO Bool -> [Picture] -> Bool -> IO [Picture]
    renderCell x y _ acc c = return $
      if c
        then Translate (fromIntegral x * 10) (fromIntegral y * 10) (Color black (circleSolid 5)) : acc
        else acc

handleEvents :: Event -> World -> IO World
handleEvents _ = return

update :: Float -> World -> IO World
update _ w = do
  instructions <- Life.lifeSimulationStepInstructions w
  mapM_ (Life.runInstructionUGrid w) instructions
  return w
