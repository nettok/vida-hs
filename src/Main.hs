module Main where

import Control.Monad

import Graphics.Gloss.Interface.IO.Game

import Life

type World = Life.UGrid IO Bool

main :: IO ()
main = do
  let window = InWindow "Vida" (width, height) (100, 50)
  let background = white
  let fps = 5
  initialState <- Life.makeEmptySquareUGrid 80
  mapM_ (Life.runInstructionUGrid initialState) (Life.createGliderInstructions 1 1)
  playIO window background fps initialState render handleEvents update

width = 800 :: Int
height = 800 :: Int

render :: World -> IO Picture
render w = do
  grid <- renderGrid w
  Pictures . (:) grid <$> Life.ifoldUGrid renderCell [] w
  where
    renderCell :: Int -> Int -> Life.NeighbourRows IO Bool -> [Picture] -> Bool -> IO [Picture]
    renderCell x y _ acc c = return $
      if c
        then newAliveCell x y : acc
        else acc

    newAliveCell :: Int -> Int -> Picture
    newAliveCell x y =
      Translate posX posY (Color black (circleSolid cellRadius))
      where
        cellRadius = 5 :: Float
        halfWidth = (fromIntegral width :: Float) / 2
        halfHeight = (fromIntegral height :: Float) / 2
        posX = fromIntegral (x * 10) - halfWidth + cellRadius
        posY = fromIntegral (y * (-10)) + halfHeight - cellRadius

    renderGrid :: World -> IO Picture
    renderGrid w =
      -- TODO: paint the real grid
      return $ Pictures [Color red $ circleSolid 15, Color green $ Circle 20]

handleEvents :: Event -> World -> IO World
handleEvents _ = return

update :: Float -> World -> IO World
update _ w = do
  instructions <- Life.lifeSimulationStepInstructions w
  mapM_ (Life.runInstructionUGrid w) instructions
  return w
