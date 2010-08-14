module Sprite.Ship where

import Graphics.Rendering.OpenGL
import Sprite.Class
import Sprite.Colors

data Ship = Ship (Vector3 Float) Float

instance Sprite Ship where
  pos (Ship p v) = p

  draw (Ship p v) = do
    preservingMatrix $ do
      translate p
      rotate (45 * v) (Vector3 0 1 0)
      scale 0.1 0.1 (0.1::Float)
      renderPrimitive Triangles $ do
        materialAmbientAndDiffuse Front $= blue
        materialSpecular Front $= Color4 0.8 0.8 0.8 1.0
        materialShininess Front $= 128
        vertex $ Vertex3 0 1 (0::Float)
        vertex $ Vertex3 (-1) (-1) (0::Float)
        vertex $ Vertex3 1 (-1) (0::Float)

  update (Ship p v) = let Vector3 x y z = p
                          x' = x + 0.02 * v
                          p' = Vector3 x' y z
                      in Ship p' v
