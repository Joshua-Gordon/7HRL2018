module Level where

import System.Random
import Control.Monad

type Level = [[Tile]]

data Tile = Floor | Wall | Object --Have Object include function for interact

randBool :: IO Boolean
randBool = do
          g <- getStdGen
          let x = randomR (0,1) g
          return x == 1

genLevel :: Int -> Int -> IO Level
genLevel x y = do
              g <- getStdGen
