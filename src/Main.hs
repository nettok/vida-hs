module Main where

import Control.Monad (mapM_)
import Data.List (iterate)

import Graphics.Gloss.Interface.IO.Game

import Life

type World = Life.UGrid IO Bool

main :: IO ()
main = do
  let window = InWindow "Vida" (width, height) (100, 50)
  let background = white
  let fps = 5
  let gridSize = width `div` (round cellRadius * 2)
  initialState <- Life.makeEmptySquareUGrid gridSize
  mapM_ (Life.runInstructionUGrid initialState) (Life.createGliderInstructions 1 1)
  playIO window background fps initialState render handleEvents update

width = 800 :: Int
height = width :: Int

halfWidth = (fromIntegral width :: Float) / 2
halfHeight = (fromIntegral height :: Float) / 2

cellRadius = 5 :: Float

render :: World -> IO Picture
render w = do
  (maxX, maxY) <- Life.ugridBounds w
  grid <- renderGrid (fromIntegral maxX) (fromIntegral maxY) w
  Pictures . (:) grid <$> Life.ifoldUGrid renderCell [] w
  where
    renderCell :: Int -> Int -> Life.NeighbourRows IO Bool -> [Picture] -> Bool -> IO [Picture]
    renderCell x y _ acc c = return $
      if c
        then renderAliveCell x y : acc
        else acc

    renderAliveCell :: Int -> Int -> Picture
    renderAliveCell x y =
      Translate posX posY (Color black (circleSolid cellRadius))
      where
        posX = (fromIntegral x * (cellRadius * 2)) - halfWidth + cellRadius
        posY = (fromIntegral y * (-cellRadius * 2)) + halfHeight - cellRadius

    renderGrid :: Float -> Float -> World -> IO Picture
    renderGrid maxX maxY w =
      return $
        Translate (-halfWidth) (-halfHeight) $
          Color (greyN 0.75) $
            Pictures [
              Pictures $ map renderXLine [0 .. maxX + 1]
            , Pictures $ map renderYLine [0 .. maxY + 1]
            ]
      where
        xMult = fromIntegral width / (maxX + 1)
        yMult = fromIntegral height / (maxY + 1)
        renderXLine x = Line [(x * xMult, 0), (x * xMult, (maxY + 1) * yMult)]
        renderYLine y = Line [(0, y * yMult), ((maxX + 1) * xMult, y * yMult)]

handleEvents :: Event -> World -> IO World
handleEvents _ = return

update :: Float -> World -> IO World
update _ w = do
  instructions <- Life.lifeSimulationStepInstructions w
  mapM_ (Life.runInstructionUGrid w) instructions
  return w
