module Level where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import System.Random
import Control.Monad

import Debug.Trace

import Util
import {-# SOURCE #-} Mob

type Level = [[Tile]]

data World = Overworld Level Player [Monster] | Battle World Player [Monster]

handleInput :: Event -> World -> IO World
handleInput (EventKey (SpecialKey KeyUp) Graphics.Gloss.Interface.IO.Game.Down _ _) (Overworld l p ms) = do print "up"; return $ Overworld l (movePlayer Util.Up p) ms
handleInput (EventKey (SpecialKey KeyDown) Graphics.Gloss.Interface.IO.Game.Down _ _) (Overworld l p ms) = return $ Overworld l (movePlayer Util.Down p) ms
handleInput (EventKey (SpecialKey KeyLeft) Graphics.Gloss.Interface.IO.Game.Down _ _) (Overworld l p ms) = return $ Overworld l (movePlayer Util.Left p) ms
handleInput (EventKey (SpecialKey KeyRight) Graphics.Gloss.Interface.IO.Game.Down _ _) (Overworld l p ms) = return $ Overworld l (movePlayer Util.Right p) ms
handleInput _ w = return w

data Tile = Floor | Wall | Object (Player -> Player) --Have Object include function for interact

instance Eq Tile where
  Floor == Floor = True
  Wall == Wall = True
  Floor == Wall = False
  Wall == Floor = False


randBool :: IO Bool
randBool = do
          g <- newStdGen
          let (x,_) = randomR (0,2) g :: (Int,StdGen)
          return $ x == 1



genLevel :: Int -> Int -> IO Level
genLevel x y = do
              boolsflat <- replicateM (x*y) randBool
              let bools = init $ chunk x boolsflat
              let levelFirst = [[if (bools !! n !! m) then Floor else Wall | m <- [0..x-1]] | n <- [0..y-1]]
              return $ generate 100 levelFirst

(!!!) :: Maybe [a] -> Int -> Maybe a
Nothing !!! _ = Nothing
(Just (x:xs)) !!! n | n == 0 = Just x
                    | null xs = Nothing
                    | otherwise = (Just xs) !!! (n-1)

neighbors :: Int -> Int -> Level -> Int
neighbors x y l = let range = [(x-1,y-1),(x,y-1),(x+1,y-1),(x-1,y),(x,y),(x+1,y),(x-1,y+1),(x,y+1),(x+1,y+1)]
                      ns = filter (/=Nothing) [Just l !!! xr !!! yr | (xr,yr) <- range]
                  in length $ filter (/=(Just Wall)) ns

step :: Int -> Int -> Level -> Tile
step x y l = let n = neighbors x y l
             in if n == 3 then Floor else if n == 0 then Wall else if n < 6 then l !! x !! y else Wall

--runs the cellular automaton
generate :: Int -> Level -> Level
generate n l | n == 0 = l
             | otherwise = generate (n-1) [[step x y l | x <- [0..length l - 1]] | y <- [0..length (head l) - 1]]

tile :: Level -> Pos -> Maybe Tile
tile l p = Just l !!! (fst p) !!! (snd p)

width :: Level -> Int
width l = length (head l)

height :: Level -> Int
height = length
