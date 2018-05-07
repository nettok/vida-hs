module Main where

import Control.Monad as M
import Control.Monad.Primitive (PrimMonad, PrimState)
-- import Control.Monad.ST
import Data.Maybe (isJust, fromJust)
import Data.Vector.Fusion.Stream.Monadic as MS hiding ((++))
import Data.Vector.Generic.Mutable as MGV
import Data.Vector.Mutable as MV
import Data.Vector.Unboxed.Mutable as MUV

newtype UGrid m a = UGrid (MV.MVector (PrimState m) (MUV.MVector (PrimState m) a))

data NeighbourRows m a = NeighbourRows
  { aboveRow :: Maybe (MUV.MVector (PrimState m) a)
  , thisRow :: MUV.MVector (PrimState m) a
  , belowRow :: Maybe (MUV.MVector (PrimState m) a)
  }

data Instruction a = Instruction Int Int (CellOperation a)

data CellOperation a =
    SetCell a
  | TransformCell (a -> a)

main :: IO ()
main = do
  g <- makeEmptySquareUGrid 3
  printUGrid g
  putStrLn "---"
  M.mapM_ (runInstructionUGrid g) (createGliderInstructions 1 1)
  printUGrid g
  let UGrid vv = g
  v <- MGV.read vv 0
  count <- ifoldUGrid (\x y _ n c -> return $ if c then n + 1 else n) 0 g
  putStrLn $ "Trues: " ++ show count
  -- return ()

makeEmptySquareUGrid :: (PrimMonad m) => Int -> m (UGrid m Bool)
makeEmptySquareUGrid n = fmap UGrid (MGV.replicateM n (MGV.replicate n False))

printUGrid :: (Show a, Unbox a) => UGrid IO a -> IO ()
printUGrid = imapUGrid (\x y _ c -> putStrLn (show x ++ "_" ++ show y ++ " " ++ show c))

imapUGrid :: (PrimMonad m, Unbox a) => (Int -> Int -> MUV.MVector (PrimState m) a -> a -> m ()) -> UGrid m a -> m ()
imapUGrid f (UGrid vv) = imapMGV (\y v -> (imapMGV (\x c -> f x y v c) v)) vv

imapMGV :: (PrimMonad m, MGV.MVector v a) => (Int -> a -> m ()) -> v (PrimState m) a -> m ()
imapMGV f v = MS.mapM_ (uncurry f) (MS.indexed $ MGV.mstream v)

ifoldUGrid :: (PrimMonad m, Unbox b) => (Int -> Int -> NeighbourRows m b -> a -> b -> m a) -> a -> UGrid m b -> m a
ifoldUGrid f a (UGrid vv) = ifoldMGV (\y a1 v ->
                                        (neighbourRows vv y >>= \nr ->
                                            ifoldMGV (\x a2 c -> f x y nr a2 c) a1 v))
                                     a vv
  where
    neighbourRows :: (PrimMonad m, Unbox b) => MV.MVector (PrimState m) (MUV.MVector (PrimState m) b) -> Int -> m (NeighbourRows m b)
    neighbourRows vv y = do
      let maxY = MGV.length vv - 1
      above <- if y <= 0 then return Nothing else Just <$> MGV.read vv (y - 1)
      this <- MGV.read vv y
      below <- if y >= maxY then return Nothing else Just <$> MGV.read vv (y + 1)
      return NeighbourRows { aboveRow = above, thisRow = this , belowRow = below }

ifoldMGV :: (PrimMonad m, MGV.MVector v b) => (Int -> a -> b -> m a) -> a -> v (PrimState m) b -> m a
ifoldMGV f a v = MS.foldM (\a1 (i, c) -> f i a1 c) a (MS.indexed $ MGV.mstream v)

runInstructionUGrid :: (PrimMonad m, Unbox a) => UGrid m a -> Instruction a -> m ()
runInstructionUGrid (UGrid vv) (Instruction x y op) = do
  v <- MGV.read vv y
  case op of
    SetCell c       -> MGV.write v x c
    TransformCell f -> MGV.read v x >>= \c -> MGV.write v x (f c)

data LifeSimulationAcc = LifeSimulationAcc
  { maxX :: Int
  , maxY :: Int
  , instructionsAcc :: [Instruction Bool]
  }

lifeSimulationStepInstructions :: (PrimMonad m) => UGrid m Bool -> m [Instruction Bool]
lifeSimulationStepInstructions g = do
  (maxX, maxY) <- ugridBounds g
  let initialState = LifeSimulationAcc { maxX = maxX, maxY = maxY, instructionsAcc = [] }
  instructionsAcc <$> ifoldUGrid iteration initialState  g
  where
    iteration :: (PrimMonad m) => Int -> Int -> NeighbourRows m Bool -> LifeSimulationAcc -> Bool -> m LifeSimulationAcc
    iteration x y nr (lsa@(LifeSimulationAcc maxX maxY acc)) c = do
      neighbourCells <- readNeighbourCells x nr
      let aliveNeighbourCells = countTrue neighbourCells
      return $ case instruction x y c aliveNeighbourCells of
                  Just newInstruction -> lsa { instructionsAcc = newInstruction : instructionsAcc lsa }
                  Nothing -> lsa

    instruction :: Int -> Int -> Bool-> Int -> Maybe (Instruction Bool)
    instruction x y c aliveNeighbourCells
      -- cell is alive
      | c && aliveNeighbourCells < 2 = Just $ Instruction x y (SetCell False)  -- die of loneliness
      | c && aliveNeighbourCells > 3 = Just $ Instruction x y (SetCell False)  -- die of overpopulation
      | c                            = Nothing
      -- cell is dead
      | not c && aliveNeighbourCells == 3 = Just $ Instruction x y (SetCell True)  -- cell borns
      | otherwise                         = Nothing

ugridBounds :: (PrimMonad m, Unbox a) => UGrid m a -> m (Int, Int)
ugridBounds (UGrid vv) = do
  v <- MGV.read vv 0
  return (MGV.length v - 1, MGV.length vv - 1)

readNeighbourCells :: (PrimMonad m, Unbox a) => Int -> NeighbourRows m a -> m [a]
readNeighbourCells x nr = do
  aboveCells <- readMaybeRow (aboveRow nr)
  thisCells <- readThisRow (thisRow nr)
  belowCells <- readMaybeRow (belowRow nr)
  return $ fromJust $ (>>= id) <$> sequence (Prelude.filter isJust [aboveCells, Just thisCells, belowCells])
  where
    readMaybeRow :: (PrimMonad m, Unbox a) => Maybe (MUV.MVector (PrimState m) a) -> m (Maybe [a])
    readMaybeRow r = traverse sequence $ fmap (sequence cellReads) r
      where cellReads = [flip MGV.read (x - 1), flip MGV.read x, flip MGV.read (x + 1)]

    readThisRow :: (PrimMonad m, Unbox a) => MUV.MVector (PrimState m) a -> m [a]
    readThisRow r = sequence (cellReads <*> [r])
      where cellReads = [flip MGV.read (x - 1), flip MGV.read (x + 1)]

countTrue :: [Bool] -> Int
countTrue = Prelude.foldl (\n c -> if c then n + 1 else n) 0

createGliderInstructions :: Int -> Int -> [Instruction Bool]
createGliderInstructions x y = [
    Instruction (x - 1) (y - 1) (SetCell False)
  , Instruction (x + 0) (y - 1) (SetCell True)
  , Instruction (x + 1) (y - 1) (SetCell False)
  , Instruction (x - 1) (y + 0) (SetCell False)
  , Instruction (x + 0) (y + 0) (SetCell False)
  , Instruction (x + 1) (y + 0) (SetCell True)
  , Instruction (x - 1) (y + 1) (SetCell True)
  , Instruction (x + 0) (y + 1) (SetCell True)
  , Instruction (x + 1) (y + 1) (SetCell True)
  ]
