module Main where

import System.Random
import System.Process

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import Level
import Screen
import Mob
import Battle
import Util




main :: IO ()
--main = sMain
main = do
  system "cd intro; python2 release.py"
  lv' <- genLevel 33 59
  let lv = boundLevel lv'
  alien <- spaceman (5,5)
  let testWorld = Overworld lv startingPlayer [alien]
  let testbattle = Battle testWorld startingPlayer [alien] (True,True) (State None AttackM) True
  playIO FullScreen red 5 testbattle renderWorld handleInput idstep

idstep _ w@(Overworld l p lm) = do
  let newMonsters = map (\m -> monsterThink m w) lm
  let pp = pos p
  let cols = filter (\m -> posM m == pp) newMonsters
  if not $ null $ filter (\m -> nameM m == "stairs") cols then do
    lv' <- genLevel 33 59
    let lv = boundLevel lv'
    alien <- spaceman (7,5)
    let testWorld = Overworld lv startingPlayer [alien{nameM="stairs"},alien{posM=(10,7)}]
    return testWorld
  else do
  return $ Overworld l p newMonsters
idstep f b = stepBattle f b

stepBattle :: Float -> World -> IO World
stepBattle _ here@(Battle w p ms sel (State pa ma) turn) | null ms = return w
                                                    | pa == None = return here
                                                    | otherwise = if turn then case pa of
                                                      Run -> return w
                                                      Attack -> do
                                                        print "Attack!"
                                                        g <- getStdGen
                                                        let (idx,g') = choice g [0..length ms-1]-- :: (Int,StdGen)
                                                        let m = attackMob p (ms !! idx)
                                                        print "Attacked"
                                                        return $ case m of
                                                          Just m' -> Battle w p (replace ms idx m') sel (State None ma) False
                                                          Nothing -> Battle w p (remove ms idx) sel (State None ma) False
                                                      --Item _ -> return here
                                                      --Ability _ -> return here
                                                      else return here
