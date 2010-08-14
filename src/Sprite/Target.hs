module Sprite.Target where

import Graphics.Rendering.OpenGL
import Sprite.Class
import Sprite.Colors

data Target = Target (Vector3 Float)

instance Sprite Target where

  pos (Target v) = v

  draw (Target v) = do
    preservingMatrix $ do
      translate v
      scale 0.1 0.1 (0.1::Float)
      renderPrimitive Quads $ do
        materialAmbientAndDiffuse Front $= black
        vertex (Vertex3 ( 0.5) ( 0.5) 0 :: Vertex3 Float)
        vertex (Vertex3 (-0.5) ( 0.5) 0 :: Vertex3 Float)
        vertex (Vertex3 (-0.5) (-0.5) 0 :: Vertex3 Float)
        vertex (Vertex3 ( 0.5) (-0.5) 0 :: Vertex3 Float)


  update (Target v) = Target $ Vector3 x' y' z
    where
      Vector3 x y z = v
      x' = x + (sin (20*y))/100
      y' = (y - 0.005)
