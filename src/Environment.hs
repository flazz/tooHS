module Environment where

import Control.Monad
import Graphics.Rendering.OpenGL.GL
import Graphics.Rendering.OpenGL.GLU

type Vert = Vertex3 Float
type Strip = [(Vert, Vert)]
type Terrain = [Strip]
type Noise = (Float, Float, Float)

seed :: [Vert]
seed = [ Vertex3 (x - 2) (-2) 0 | x <- [0, 0.5 ..] ]

-- | Move a vertex on the y axis
moveY :: Float -> Vert -> Vert
moveY d (Vertex3 x y z) = Vertex3 x (y + d) z

-- | Wiggle a vertex with some noise
wiggle :: Noise -> Vert -> Vert
wiggle (a, b, c) (Vertex3 x y z) = Vertex3 x' y' z'
  where x' = x + a
        y' = y + b
        z' = z + c

nextVerts :: [Noise] -> [Vert] -> [Vert]
nextVerts ns vs = zipWith3 f ns (repeat 0.5) vs
  where f n dy v = (wiggle n) . (moveY dy) $ v

initialStrip :: [Noise] -> Strip
initialStrip ns = take 9 $ zip seed (nextVerts ns seed)

nextStrip :: [Noise] -> Strip -> Strip
nextStrip ns vs = zip bs bs'
  where (_as, bs) = unzip vs
        bs' = nextVerts ns bs

mkNoise :: [Float] -> Noise
mkNoise fs = zip3 as bs cs
  where (as, r1) = splitAt 9 fs
        (bs, r2) = splitAt 9 r1
        cs = take 9 r2

initialTerrain :: [Noise] -> Terrain
initialTerrain ns = take 8 $ iterate (nextStrip ns) (initialStrip ns)

flatten :: Strip -> [Vert]
flatten s = concat [ [a, b] | (a, b) <- s ]

drawTerrain :: Terrain -> IO ()
drawTerrain ss = do
  preservingMatrix $ do
    loadIdentity
    translate (Vector3 0 0 (-5::Float))
    materialAmbientAndDiffuse FrontAndBack $= green
    mapM_ renderStrip ss
  return ()
  where green = Color4 0 1 0 (0.5) :: Color4 GLclampf
        renderStrip s = renderPrimitive TriangleStrip $ mapM_ vertex vs
          where vs = flatten s
