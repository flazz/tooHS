module Game where

import Graphics.Rendering.OpenGL.GL
import Graphics.Rendering.OpenGL.GLU

import Sprite

data Game = Game { ship :: Ship
                 , shots :: [Shot]
                 , targets :: [Target]
                 }

initialGame = Game { ship = (Ship (Vector3 0 (-0.6) 0) 0)
                   , shots = []
                   , targets = []
                   }

updateGame :: Game -> Game
updateGame g = g { ship = (update . ship) g }

go :: Float -> Game -> Game
go v g = let Ship p _ = ship g
         in g { ship = Ship p v }
