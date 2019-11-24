module Squares where

import Data.Matrix
import Data.Functor ((<&>))

data Square = White | Red

instance Show Square where
  show White = "o"
  show Red   = "X"

type Grid = Matrix Square

startingGrid :: Grid
startingGrid = matrix 5 5 (const White)

touchSquare :: (Int, Int) -> Grid -> Grid
touchSquare (row, col) grid =
  let touching = (row, col)
      above    = (row, col + 1)
      below    = (row, col - 1)
      left     = (row - 1, col)
      right    = (row + 1, col)
  in
     ( flipSquareGrid touching
     . flipSquareGrid above
     . flipSquareGrid below
     . flipSquareGrid left
     . flipSquareGrid right
     ) grid


flipSquareGrid :: (Int, Int) -> Grid -> Grid
flipSquareGrid (row, col) grid =
  let cellValue = safeGet row col grid
  in  case cellValue of
    Just square -> setElem (flipSquare square) (row, col) grid
    Nothing     -> grid

flipSquare :: Square -> Square
flipSquare White = Red
flipSquare Red = White

main :: IO ()
main = run startingGrid

run :: Grid -> IO ()
run grid = do
  pos <- getLine <&> \i -> read $ '(' : i ++ ")"
  let newGrid = touchSquare pos grid
  putStrLn $ prettyMatrix newGrid
  run newGrid
