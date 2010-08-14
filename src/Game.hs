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
                 , shots = (dropWhile offScreen . map update) $ shots g
                 }

  where offScreen (Shot (Vector3 x y z)) = y > 0.5

go :: Float -> Game -> Game
go v g = let Ship p _ = ship g
         in g { ship = Ship p v }

fireShot :: IORef Game -> IO ()
fireShot gameRef = do
  modifyIORef gameRef f
  where f g = let Ship (Vector3 x y z) v = ship g
                  p = Vector3 x (y + 0.1) z
              in g { shots = (Shot p):(shots g) }

keepShooting :: IORef Game -> IO ()
keepShooting gameRef = do
  game <- readIORef gameRef
  when (shooting game) $ do
    fireShot gameRef
    addTimerCallback delay $ keepShooting gameRef
  where delay = 100

stopShooting :: IORef Game -> IO ()
stopShooting gameRef = modifyIORef gameRef f
  where f g = g { shooting = False }

scrollTerrain :: IORef Game -> IO ()
scrollTerrain gameRef = do
  game <- readIORef gameRef
  let game' = game { terrain = (update . terrain) game }
  writeIORef gameRef game'
  postRedisplay Nothing
  addTimerCallback 10 $ scrollTerrain gameRef
