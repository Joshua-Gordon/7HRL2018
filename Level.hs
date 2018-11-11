module Level where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import System.Random
import Control.Monad
import Data.List

import Debug.Trace

import Util
import {-# SOURCE #-} Mob

type Level = [[Tile]]

data World = Overworld Level Player [Monster] | Battle World Player [Monster] (Bool,Bool)

handleInput :: Event -> World -> IO World
handleInput (EventKey (SpecialKey KeyUp) Graphics.Gloss.Interface.IO.Game.Down _ _) (Overworld l p ms) = do print "up"; return $ Overworld l (movePlayer Util.Up p l) ms
handleInput (EventKey (SpecialKey KeyDown) Graphics.Gloss.Interface.IO.Game.Down _ _) (Overworld l p ms) = return $ Overworld l (movePlayer Util.Down p l) ms
handleInput (EventKey (SpecialKey KeyLeft) Graphics.Gloss.Interface.IO.Game.Down _ _) (Overworld l p ms) = return $ Overworld l (movePlayer Util.Left p l) ms
handleInput (EventKey (SpecialKey KeyRight) Graphics.Gloss.Interface.IO.Game.Down _ _) (Overworld l p ms) = return $ Overworld l (movePlayer Util.Right p l) ms
handleInput (EventKey (Char 'd') Graphics.Gloss.Interface.IO.Game.Down _ _) (Overworld l p ms) = return $ Overworld (drill l p) p ms
handleInput _ w = return w

data Tile = Floor | Wall
  deriving (Eq,Show)


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

boundLevel :: Level -> Level
boundLevel l = let sides = map (\s -> Wall : s ++ [Wall]) l
                   n = length (head sides)
                   bound = replicate n Wall
               in bound : sides ++ [bound]

drill :: Level -> Player -> Level
drill l p = let (x,y) = getPos p
                (x',y') = applyHeading (getPos p) (getHeading p) 1
			in [[if n == x' && m == y' then Floor else l!!n!!m | m <- [0 .. width l - 1]] | n <- [0 .. height l - 1]]
