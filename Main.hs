module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import Level
import Screen
import Mob




main :: IO ()
--main = sMain
main = do
  lv' <- genLevel 33 59
  let lv = boundLevel lv'
  alien <- spaceman (5,5)
  let testWorld = Overworld lv startingPlayer [alien]
  playIO FullScreen red 1 testWorld renderWorld handleInput idstep

idstep :: Float -> World -> IO World
idstep _ w@(Overworld l p lm) = do
  let newMonsters = map (\m -> monsterThink m w) lm
  return $ Overworld l p newMonsters
