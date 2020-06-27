module Main where

import Lib
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort (ViewPort)
import System.Random

type Model = [Cell]
render :: Int -> Int -> Int -> Model -> Picture
render w h cellSize state = 
    pictures $ map draw state 
    where 
        size = fromIntegral cellSize :: Float
        w_f  = fromIntegral w :: Float
        h_f  = fromIntegral h :: Float
        draw (Cell i alive) = 
            let (x_, y_) = posOfIndex i w
                x = fromIntegral x_ :: Float
                y = fromIntegral y_ :: Float
                fill = if alive then black else white
                in  translate
                    ((x - w_f/2.0)*size)
                    ((y - h_f/2.0)*size)
                    (color fill (rectangleSolid size size))

update :: Int -> Int -> ViewPort -> Float -> Model -> Model
update w h vp t state = nextGen w h state

firstGen :: Int -> Int -> Int -> Cell
firstGen w h i =
    Cell i alive
        where 
            x = i `mod` w - w `quot` 2
            y = i `quot` h - h `quot` 2
            alive = 
                sqrt (fromIntegral (x^2 + y^2) :: Float) < 10
                || i `mod` 5 == 1
            

main :: IO ()
main =
    let 
        (width, height) = (800, 600)
        cellSize = 10
        (w, h) = (width `quot` cellSize, height `quot` cellSize)
        in simulate 
            (InWindow 
                "Conway's Game of Life"
                (width, height)
                (10, 10))
            white
            5
            [ firstGen w h i | i <- [0..(w * h- 1)] ]
            (render w h cellSize)
            (update w h)
