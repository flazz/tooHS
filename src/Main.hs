module Main where

import Graphics.Rendering.OpenGL.GL
import Graphics.Rendering.OpenGL.GLU
import qualified Graphics.UI.GLUT as GLUT
import Data.IORef
import System.Random

import Game
import Sprite
import Input

main :: IO ()
main = do

  GLUT.initialDisplayMode $= [ GLUT.DoubleBuffered
                             , GLUT.RGBAMode
                             , GLUT.WithDepthBuffer
                             ]

  GLUT.initialWindowSize $= Size 800 800
  (_progname, _args) <- GLUT.getArgsAndInitialize
  Size xres yres <- get GLUT.screenSize
  GLUT.createWindow "toohs"

  gen <- newStdGen
  let rs = randomRs (-0.5, 0.5) gen

  gameRef <- newIORef $ (initialGame rs)

  GLUT.globalKeyRepeat $= GLUT.GlobalKeyRepeatOff

  GLUT.displayCallback $= (display gameRef)
  GLUT.keyboardMouseCallback $= Just (handleInput gameRef)
  GLUT.idleCallback $= Just (handleIdle gameRef)
  scrollTerrain gameRef
  GLUT.mainLoop

  where
    handleInput gameRef key state _ _ = input gameRef key state
    handleIdle gameRef = do
        modifyIORef gameRef updateGame
        GLUT.postRedisplay Nothing

display :: IORef Game -> IO ()
display gameRef = do
  setDrawingFeatures
  clear [ColorBuffer, DepthBuffer]

  setProjectionMatrix
  setModelMatrix
  setLighting

  game <- readIORef gameRef

  (draw . terrain) game
  translate (Vector3 0 0 (-2) :: Vector3 Float)
  (draw . ship) game
  mapM_ draw (shots game)
  GLUT.swapBuffers

  where
    setDrawingFeatures = do
      clearColor $= white
      blend $= Enabled
      blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
      {-depthFunc $= Just Less-}
      depthFunc $= Just Lequal
      where black = Color4 0 0 0 1 :: Color4 GLclampf
            white = Color4 1 1 1 1 :: Color4 GLclampf

    setProjectionMatrix = do
      matrixMode $= Projection
      loadIdentity
      perspective 45 1 1 1000

    setModelMatrix = do
      matrixMode $= Modelview 0
      loadIdentity

    setLighting = do
      normalize $= Enabled
      shadeModel $= Flat

      lighting $= Enabled
      lightModelTwoSide $= Enabled
      light (Light 0) $= Enabled

      ambient (Light 0) $= (Color4 0.2 0.2 0.2 1)
      diffuse (Light 0) $= (Color4 0.8 0.8 0.8 1)
      specular (Light 0) $= (Color4 1 1 1 1)
      position (Light 0) $= Vertex4 1 0 1 (1.0)
