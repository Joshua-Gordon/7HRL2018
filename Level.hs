module Level where

import System.Random
import Control.Monad

type Level = [[Tile]]

data Tile = Floor | Wall | Object --Have Object include function for interact

randBool :: IO Bool
randBool = do
          g <- getStdGen
          let (x,_) = randomR (0,1) g :: (Int,StdGen)
          return $ x == 1

chunk :: Int -> [a] -> [[a]]
chunk n xs = take n xs : chunk n (drop n xs)

genLevel :: Int -> Int -> IO Level
genLevel x y = do
              boolsflat <- replicateM (x*y) randBool
              let bools = chunk x boolsflat
              let levelFirst = [[if (bools !! m !! n) then Floor else Wall | m <- [0..x]] | n <- [0..y]]
              return $ generate 100 levelFirst

--runs the cellular aut
generate :: Int -> Level -> Level
generate = undefined
