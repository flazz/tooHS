module Sprite where

import Graphics.Rendering.OpenGL.GL
import Graphics.Rendering.OpenGL.GLU

black = Color4 0 0 0 1 :: Color4 GLclampf
red = Color4 1 0 0 1 :: Color4 GLclampf

class Sprite a where
  draw :: a -> IO ()
  update :: a -> a

data Ship = Ship (Vector3 Float) Float

instance Sprite Ship where
  draw (Ship p v) = do
    pointSize $= 5
    lineWidth $= 2
    color black
    preservingMatrix $ do
      translate p
      rotate (45 * v) (Vector3 0 1 0)
      scale 0.1 0.1 (0.1::Float)
      renderPrimitive Points $ mapM_ vertex points
      renderPrimitive LineLoop $ mapM_ vertex points
    where
      points :: [Vertex3 Float]
      points = [ Vertex3 0 1 0
               , Vertex3 (-1) (-1) 0
               , Vertex3 1 (-1) 0
               ]
  update (Ship p v) = let Vector3 x y z = p
                          x' = x + 0.003 * v
                          p' = Vector3 x' y z
                      in Ship p' v

data Shot = Shot (Vector3 Float)
instance Sprite Shot where
  draw (Shot v) = do
    translate v
    renderPrimitive Points $ do
      vertex (Vertex3 0 0 0 :: Vertex3 Float)
  update = id

data Target = Target (Vector3 Float)
instance Sprite Target where
  draw (Target v) = do
    translate v
    renderPrimitive Points $ do
      vertex (Vertex3 0 0 0 :: Vertex3 Float)
  update = id
