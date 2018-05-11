module Game where

import Control.Monad (mapM_, unless)

import Graphics.Gloss.Interface.IO.Game

import Life

data World = World
  { wGrid :: Grid
  , wPaused :: Bool
  }

type Grid = Life.UGrid IO Bool

width = 800 :: Int
height = width :: Int

halfWidth = (fromIntegral width :: Float) / 2
halfHeight = (fromIntegral height :: Float) / 2

cellRadius = 5 :: Float

render :: World -> IO Picture
render w = do
  let wg = wGrid w
  (maxX, maxY) <- Life.ugridBounds wg
  grid <- renderGrid (fromIntegral maxX) (fromIntegral maxY) wg
  Pictures . (:) grid <$> Life.ifoldUGrid renderCell [] wg
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

    renderGrid :: Float -> Float -> Grid -> IO Picture
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
handleEvents e w =
  case e of
    EventKey (SpecialKey KeySpace) Up _ _ -> onPauseKey e w
    _ -> return w

update :: Float -> World -> IO World
update _ w = do
  let wg = wGrid w
  unless (wPaused w) $ do
    instructions <- Life.lifeSimulationStepInstructions wg
    mapM_ (Life.runInstructionUGrid wg) instructions
  return w

-----------------
-- Event handlers

onPauseKey :: Event -> World -> IO World
onPauseKey e w = return $ w { wPaused = not $ wPaused w }
