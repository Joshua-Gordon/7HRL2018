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
  let alien = spaceman (5,5)
  let testWorld = Overworld lv startingPlayer [alien]
  playIO FullScreen red 1 testWorld renderWorld handleInput idstep

idstep :: Float -> World -> IO World
idstep _ w = return w

stepBattle :: Float -> World -> IO World
stepBattle _ here@(Battle w p ms sel (State pa ma)) | null ms = return w
                                                    | pa == None = return here
                                                    | otherwise = case pa of
                                                      Run -> return w
                                                      Attack -> do
                                                        enemy <- choice mkStdGen ms
