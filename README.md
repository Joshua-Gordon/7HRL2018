# 7HRL2018
Vent Crawler (K or Q optional)
Vent Crawler


Software Design
Modules & Files:
Main
  Main.hs
Level
  Level.hs (contains proc gen code, implements world step)
  Hazard.hs (uses event, runs one of several possible hazard events defined here)
Event
  Event.hs (contains code to interrupt the real-time game)
  Battle.hs (contains code to run battle)
Screen
  Screen.hs (a screen has a list of monsters and environment objects, and is in charge of drawing all of these into an image)
Mob
  Player.hs (player info and stats)
  Monster.hs (proc gen for monsters, typeclass for Monster that includes player)

This will change as time goes on

Game Design
Time moves when the player moves. Player moves are allowed minimum .3 seconds apart. The game is in screens like the original LOZ, 1920x1080 pixels. 
A screen has a [[Tile]] and a Tile is either Floor, Object, or Wall.
A screen has a [Monster], and is in charge of stepping all of them.
Monsters rarely have chaser AI, and mill about until the player gets too close.
When the player touches a monster, the Screen returns a flag to the Level telling it how to start the battle.
Battles are turn based, and end when enemies are dead. Turn order is determined by speed plus or minus a random number. The player can attack, use an item, use an ability (gained through interacting with environment and finding key items), block, or run away.
Attack damage is your attack plus rand% minus their defense.
Stats are attack, defense, and speed
Algorithms
http://roguebasin.roguelikedevelopment.org/index.php?title=Maze_Generation

