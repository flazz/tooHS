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
        materialAmbientAndDiffuse Front $= blue
        materialSpecular Front $= Color4 0.8 0.8 0.8 1.0
        materialShininess Front $= 128
        vertex $ Vertex3 0 1 (0::Float)
        vertex $ Vertex3 (-1) (-1) (0::Float)
        vertex $ Vertex3 1 (-1) (0::Float)
    where blue = Color4 0 0 1 (0.8) :: Color4 GLclampf

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

-- | terrain

green = Color4 0 1 0 1 :: Color4 GLclampf
black = Color4 0 0 0 1 :: Color4 GLclampf
white = Color4 1 1 1 1 :: Color4 GLclampf
white' = Color4 (0.1) (0.1) (0.1) (0.1) :: Color4 GLclampf

isEven x = 0 == (mod (truncate x) 2)
isOdd = not . isEven

stripPoints :: [Vertex3 Float]
stripPoints = [ Vertex3 (f y) y 0 | y <- [0..] ]
  where f n | isEven n = 0.5
            | otherwise = -0.5

wiggle ns vs = zipWith f ns' vs
  where f (a, b, c) (Vertex3 x y z) = let x' = x + a
                                          y' = y
                                          z' = sin $ z + c
                                      in Vertex3 x' y' z'
        ns' = zip3 ns (drop 1 ns) (drop 2 ns)

le :: Vertex3 Float -> Vertex3 Float
le v@(Vertex3 x y z) | isEven y = Vertex3 (x - 2) y z
                    | otherwise = v

lo :: Vertex3 Float -> Vertex3 Float
lo v@(Vertex3 x y z) | isOdd y = Vertex3 (x - 2) y z
                     | otherwise = v

re :: Vertex3 Float -> Vertex3 Float
re v@(Vertex3 x y z) | isEven y = v
                     | otherwise = Vertex3 (x + 2) y z

ro :: Vertex3 Float -> Vertex3 Float
ro v@(Vertex3 x y z) | isOdd y = v
                     | otherwise = Vertex3 (x + 2) y z

renderStrip :: [Vertex3 Float] -> IO ()
renderStrip vs = do
  let vs' = pad $ takeWhile onScreen vs
  materialAmbient Front $= green
  materialDiffuse Front $= black
  renderPrimitive TriangleStrip $ mapM_ vertex vs'
  where onScreen (Vertex3 x y z) = y <= (6.0)
        pad vs = let (Vertex3 x1 y1 z1):(Vertex3 x2 y2 _):_ = vs
                 in if x1 > x2 then (Vertex3 x2 y1 z1):vs else vs

scrollStrip :: Float -> [Vertex3 Float] -> [Vertex3 Float]
scrollStrip dy = (dropWhile offScreen) . (map moveY)
  where moveY (Vertex3 x y z) = Vertex3 x (y - dy) z
        offScreen (Vertex3 x y z) = y <= (-6.0)

data Terrain = Terrain [[Vertex3 Float]]

initialTerrain :: [Float] -> Terrain
initialTerrain ns = let ps = wiggle ns stripPoints
                        psl1 = map le ps
                        psl2 = map lo psl1
                        psl3 = map le psl2

                        psr1 = map re ps
                        psr2 = map ro psr1
                        psr3 = map re psr2
                    in Terrain [ ps
                               , psl1
                               , psl2
                               , psl3
                               , psr1
                               , psr2
                               , psr3
                               ]

instance Sprite Terrain where
  draw (Terrain ss) = do
    preservingMatrix $ do
      loadIdentity
      translate (Vector3 0 0 (-7::Float))
      mapM_ renderStrip ss
  update (Terrain ss) = Terrain ss'
    where ss' = map (scrollStrip 0.01) ss
