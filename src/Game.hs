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
                 , terrain :: Terrain
                 }

-- | the initial game state
initialGame rs = Game { ship = (Ship (Vector3 0 (-0.6) 0) 0)
                      , shots = []
                      , targets = []
                      , terrain = initialTerrain rs
                      }

-- | update ship, shots, targets & terrain
updateGame :: Game -> Game
updateGame g = g { ship = (update . ship) g
                 , shots = (updateSprites . shots) g
                 , targets = case targets g of
                               [] -> targetBatch
                               otherwise -> (updateSprites . targets) g
                 , terrain = (update . terrain) g
                 }

-- | a set of new targets
targetBatch :: [Target]
targetBatch = map Target [ Vector3 x 0.8 0 | x <- [-0.5, 0, 0.5]]

-- | go right (1), left (-1) or stay still 0
go :: Float -> Game -> Game
go v g = let Ship p _ = ship g
         in g { ship = Ship p v }

-- | fire one shot
fireShot :: IORef Game -> IO ()
fireShot gameRef = do
  modifyIORef gameRef addShot
  where addShot g = g { shots = s:(shots g) }
          where Vector3 x y z = pos . ship $ g
                s = Shot $ Vector3 x (y + 0.1) z
