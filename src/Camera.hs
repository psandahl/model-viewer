module Camera
    ( Camera (view)
    , initCamera
    ) where

import           Graphics.LWGL (GLfloat)
import           Linear        (M44, V3 (..), lookAt)

data Camera = Camera
    { view :: !(M44 GLfloat)
    } deriving Show

initCamera :: Camera
initCamera =
    Camera { view = lookAt (V3 0 2 10) (V3 0 0 0) (V3 0 1 0) }
