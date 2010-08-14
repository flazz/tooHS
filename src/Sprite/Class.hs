module Sprite.Class where

import Graphics.Rendering.OpenGL

class Sprite a where
  pos :: a -> Vector3 Float
  draw :: a -> IO ()
  update :: a -> a

  onScreen, offScreen :: a -> Bool
  onScreen s = (abs x) <= 1 && (abs y) <= 1
    where Vector3 x y z = pos s
  offScreen = not . onScreen

updateSprites :: Sprite a => [a] -> [a]
updateSprites = dropWhile offScreen . map update
