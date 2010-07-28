module Sprite where

import Graphics.Rendering.OpenGL.GL
import Graphics.Rendering.OpenGL.GLU

class Sprite a where
  draw :: a -> IO ()
  update :: a -> a

-- | ship
data Ship = Ship (Vector3 Float) Float

instance Sprite Ship where

  draw (Ship p v) = do
    preservingMatrix $ do
      translate p
      rotate (45 * v) (Vector3 0 1 0)
      scale 0.1 0.1 (0.1::Float)
      renderPrimitive Triangles $ do
        materialAmbientAndDiffuse FrontAndBack $= blue
        materialSpecular FrontAndBack $= Color4 0.8 0.8 0.8 1.0
        materialShininess FrontAndBack $= 128
        vertex $ Vertex3 0 1 (0::Float)
        materialAmbientAndDiffuse FrontAndBack $= blue2
        vertex $ Vertex3 (-1) (-1) (0::Float)
        vertex $ Vertex3 1 (-1) (0::Float)
    where blue = Color4 0 0 1 1 :: Color4 GLclampf
          blue2 = Color4 0.5 0.5 1 0.7 :: Color4 GLclampf

  update (Ship p v) = let Vector3 x y z = p
                          x' = x + 0.0005 * v
                          p' = Vector3 x' y z
                      in Ship p' v

-- | shot
data Shot = Shot (Vector3 Float)
instance Sprite Shot where

  draw (Shot p) = do
    preservingMatrix $ do
      translate p
      scale 0.02 0.02 (0.02::Float)

      let Vector3 _ d _ = p
      rotate (200 * d) (Vector3 0 1 0)

      renderPrimitive Triangles $ do
        color red
        materialAmbientAndDiffuse FrontAndBack $= red
        vertex $ Vertex3 0 1 (0::Float)
        vertex $ Vertex3 (-1) (-1) (0::Float)
        vertex $ Vertex3 1 (-1) (0::Float)

      rotate (90::Float) (Vector3 0 1 0)
      renderPrimitive Triangles $ do
        color red
        vertex $ Vertex3 0 1 (0::Float)
        vertex $ Vertex3 (-1) (-1) (0::Float)
        vertex $ Vertex3 1 (-1) (0::Float)
    where red = Color4 1 0 0 1 :: Color4 GLclampf

  update (Shot (Vector3 x y z)) = let p = Vector3 x (y + 0.001) z
                                  in Shot p

-- | target
data Target = Target (Vector3 Float)
instance Sprite Target where
  draw (Target v) = do
    translate v
    renderPrimitive Points $ do
      vertex (Vertex3 0 0 0 :: Vertex3 Float)
  update = id
