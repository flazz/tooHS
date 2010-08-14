module Sprite.Shot where

import Graphics.Rendering.OpenGL
import Sprite.Class
import Sprite.Colors

data Shot = Shot (Vector3 Float)
instance Sprite Shot where
  pos (Shot p) = p

  draw (Shot p) = do
    preservingMatrix $ do
      translate p
      scale 0.02 0.02 (0.02::Float)

      let Vector3 _ d _ = p
      rotate (300 * d) (Vector3 0 0 1)

      renderPrimitive Triangles $ do
        color red
        materialAmbientAndDiffuse FrontAndBack $= red
        vertex $ Vertex3 0 1 (0::Float)
        vertex $ Vertex3 (-1) (-1) (0::Float)
        vertex $ Vertex3 1 (-1) (0::Float)

  update (Shot v) = Shot v'
    where Vector3 x y z = v
          y' = y + 0.025
          v' = Vector3 x y' z
