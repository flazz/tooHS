module Main where

import qualified Graphics.Rendering.OpenGL.GL as GL
import qualified Graphics.Rendering.OpenGL.GLU as GL
import qualified Graphics.UI.GLUT as GLUT
import Graphics.Rendering.OpenGL.GL (($=))
import Data.IORef
import Control.Monad
import Data.Time

tau = pi * 2

toDegrees r = r * 360 / tau

data State = State { angle::Float }

main = do
  {-GLUT.initialDisplayMode $= [ GLUT.RGBAMode-}
                             {-, GLUT.Multisampling-}
                             {-, GLUT.DoubleBuffered-}
                             {-, GLUT.WithAlphaComponent-}
                             {-]-}

  GLUT.initialWindowSize $= GL.Size 800 800
  (progname, args) <- GLUT.getArgsAndInitialize
  GL.Size xres yres <- GL.get GLUT.screenSize
  GLUT.createWindow "the window !!!"

  s <- newIORef $ State 0
  GLUT.displayCallback $= (display s)
  GLUT.idleCallback $= Just (update s)
  GLUT.mainLoop

black = GL.Color4 0 0 0 1 :: GL.Color4 GL.GLclampf
red = GL.Color4 1 0 0 1 :: GL.Color4 GL.GLclampf
white = GL.Color4 1 1 1 1 :: GL.Color4 GL.GLclampf

{-gray :: Int -> [[Int]]-}
gray 0 = [[]]
gray n = [ 1 : x | x <- prev ] ++ [ (-1) : x | x <- reverse prev ]
  where prev = gray (n-1)

cubePoints :: [GL.Vertex3 Float]
cubePoints = do
  (x:y:z:_) <- gray 3
  return $ GL.Vertex3 x y z


renderCube = do
  GL.pointSize $= 8
  GL.lineWidth $= 3

  GL.preservingMatrix $ do
    GL.color black
    GL.renderPrimitive GL.Points $ mapM_ GL.vertex cubePoints
    GL.renderPrimitive GL.LineLoop $ mapM_ GL.vertex cubePoints

update stateRef = do
  s <- liftM (fromRational . toRational . utctDayTime) getCurrentTime
  stateRef $= State { angle = 60 * s }
  GLUT.postRedisplay Nothing

display stateRef = do
  GL.clearColor $= white
  GL.blend $= GL.Enabled
  GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
  GL.lineSmooth $= GL.Enabled
  GL.pointSmooth $= GL.Enabled
  GL.depthFunc $= Just GL.Less

  GL.clear [GL.ColorBuffer, GL.DepthBuffer]

  GL.matrixMode $= GL.Projection
  GL.loadIdentity
  {-GL.ortho (-1.2) 1.2 (-1.2) 1.2 (-1.2) 1.2-}
  GL.ortho2D (-1.2) 1.2 (-1.2) 1.2

  GL.matrixMode $= GL.Modelview 0
  GL.loadIdentity

  GL.translate (GL.Vector3 0 0 0 :: GL.Vector3 Float)

  GL.scale 0.5 0.5 (0.5::Float)
  s <- readIORef stateRef
  GL.rotate 10 (GL.Vector3 1 0 0 :: GL.Vector3 Float)
  GL.rotate (angle s) (GL.Vector3 0 1 0 :: GL.Vector3 Float)

  renderCube

  GLUT.swapBuffers
  GL.flush

  return ()
