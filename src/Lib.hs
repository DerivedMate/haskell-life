module Lib
    ( 
      Cell (..),
      nextGen,
      posOfIndex
    ) where

data Cell = Cell Int Bool deriving (Show)
type Neighbors = [Cell]

posOfIndex :: Int -> Int -> (Int, Int)
posOfIndex i w = (i `mod` w, i `quot` w)

neighbourRange :: Int -> Int -> (Int, Int)
neighbourRange p d = (max 0 (p - 1), min (d-1) (p+1))

getNeighbours :: [Cell] -> Int -> Int -> Cell -> Neighbors
getNeighbours cells w h (Cell i _) = 
  filter isNeighbour cells
  where 
    (ix, iy) = posOfIndex i w
    (bl, br) = neighbourRange ix w
    (bt, bb) = neighbourRange iy h
    isNeighbour (Cell j alive) =
      alive
      && j /= i
      && jx >= bl && jx <= br -- within horizontal boundaries
      && jy >= bt && jy <= bb
      where (jx, jy) = posOfIndex j w

evolve :: Neighbors -> Cell -> Cell
evolve neighbours (Cell i alive)
  | (ns < 2 || ns > 3) && alive = Cell i False 
  | ns == 3 && not alive          = Cell i True
  | otherwise                     = Cell i alive
  -- | ns <= 3 && alive              = Cell i alive
  where ns = length neighbours

nextGen :: Int -> Int -> [Cell] -> [Cell]
nextGen w h cells = map aux cells
  where aux c = evolve (getNeighbours cells w h c) c