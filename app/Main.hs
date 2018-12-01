module Main where

import Lib
import Data

main :: IO ()
main =
  let gwc = gridWithCoords grid
  in outputGrid gwc
