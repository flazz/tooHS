module Input where

import Graphics.UI.GLUT
import Data.IORef
import Control.Monad
import Game

-- | handle input from keyboard
input gameRef (Char 'j') Down = modifyIORef gameRef $ go (-1)
input gameRef (Char 'j') Up = modifyIORef gameRef $ go 0

input gameRef (Char 'k') Down = modifyIORef gameRef $ go (1)
input gameRef (Char 'k') Up = modifyIORef gameRef $ go 0

input gameRef (Char 'f') Down = do
  modifyIORef gameRef $ \g -> g {shooting = True}
  fireShot gameRef

input gameRef _ _ = return ()
