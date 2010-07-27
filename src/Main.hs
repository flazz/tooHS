module Main where

import qualified Graphics.Rendering.OpenGL.GL as GL
import qualified Graphics.Rendering.OpenGL.GLU as GL
import qualified Graphics.UI.GLUT as GLUT
import Graphics.Rendering.OpenGL.GL (($=))
import Data.IORef
import Control.Monad
import Data.Time

import Game
import Sprite

tau = pi * 2
toDegrees r = r * 360 / tau

main = do
  GLUT.initialDisplayMode $= [ GLUT.RGBAMode
                             , GLUT.Multisampling
                             , GLUT.DoubleBuffered
                             , GLUT.WithAlphaComponent
                             ]


  GLUT.initialWindowSize $= GL.Size 800 800
  (progname, args) <- GLUT.getArgsAndInitialize
  GL.Size xres yres <- GL.get GLUT.screenSize
  GLUT.createWindow "the window !!!"

  gameRef <- newIORef $ initialGame

  GLUT.displayCallback $= (display gameRef)
  GLUT.idleCallback $= Just (updateState gameRef)
  GLUT.keyboardMouseCallback $= Just (input gameRef)
  GLUT.mainLoop

black = GL.Color4 0 0 0 1 :: GL.Color4 GL.GLclampf
red = GL.Color4 1 0 0 1 :: GL.Color4 GL.GLclampf
white = GL.Color4 1 1 1 1 :: GL.Color4 GL.GLclampf

updateState :: IORef Game -> IO ()
updateState gameRef = do
    modifyIORef gameRef updateGame
    GLUT.postRedisplay Nothing

input gameRef (GLUT.SpecialKey GLUT.KeyLeft) GLUT.Down _ _ = modifyIORef gameRef $ go (-1)
input gameRef (GLUT.SpecialKey GLUT.KeyLeft) GLUT.Up _ _ = modifyIORef gameRef $ go 0
input gameRef (GLUT.SpecialKey GLUT.KeyRight) GLUT.Down _ _ = modifyIORef gameRef $ go 1
input gameRef (GLUT.SpecialKey GLUT.KeyRight) GLUT.Up _ _ = modifyIORef gameRef $ go 0
input gameRef _ _ _ _ = return ()

display stateRef = do
  setDrawingFeatures
  GL.clear [GL.ColorBuffer, GL.DepthBuffer]

  setProjectionMatrix
  setModelMatrix

  GL.translate (GL.Vector3 0 0 0 :: GL.Vector3 Float)

  state <- readIORef stateRef
  (draw . ship) state

  GL.flush
  GLUT.swapBuffers

  where

    setDrawingFeatures = do
      GL.clearColor $= white
      GL.blend $= GL.Enabled
      GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
      GL.lineSmooth $= GL.Enabled
      GL.pointSmooth $= GL.Enabled
      GL.depthFunc $= Just GL.Less

    setProjectionMatrix = do
      GL.matrixMode $= GL.Projection
      GL.loadIdentity
      GL.ortho2D (-1.2) 1.2 (-1.2) 1.2

    setModelMatrix = do
      GL.matrixMode $= GL.Modelview 0
      GL.loadIdentity
