module Game where

import Graphics.Rendering.OpenGL.GL
import Graphics.Rendering.OpenGL.GLU
import Graphics.UI.GLUT
import Data.IORef
import Control.Monad

import Sprite

data Game = Game { ship :: Ship
                 , shots :: [Shot]
                 , targets :: [Target]
                 , shooting :: Bool
                 , terrain :: Terrain
                 }

initialGame rs = Game { ship = (Ship (Vector3 0 (-0.6) 0) 0)
                       , shots = []
                       , targets = []
                       , shooting = False
                       , terrain = initialTerrain rs
                       }

updateGame :: Game -> Game
updateGame g = g { ship = (update . ship) g
                 , shots = (updateSprites . shots) g
                 , targets = case targets g of
                             [] -> targetBatch
                             otherwise -> (updateSprites . targets) g
                 , terrain = (update . terrain) g
                 }
  where
    targetBatch = [ Target (Vector3 (   0) (0.8) 0)
                  , Target (Vector3 ( 0.5) (0.8) 0)
                  , Target (Vector3 (-0.5) (0.8) 0)
                  ]

go :: Float -> Game -> Game
go v g = let Ship p _ = ship g
         in g { ship = Ship p v }

fireShot :: IORef Game -> IO ()
fireShot gameRef = do
  modifyIORef gameRef f
  where f g = let Ship (Vector3 x y z) v = ship g
                  newShot = Shot (Vector3 x (y + 0.1) z)
              in g { shots = newShot:(shots g) }
