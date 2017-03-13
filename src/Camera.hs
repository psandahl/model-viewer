module Camera
    ( Camera (view)
    , initCamera
    , moveForward
    , moveBackward
    ) where

import           Graphics.LWGL (GLfloat)
import           Linear        (M44, V3 (..), lookAt)

data Camera = Camera
    { view :: !(M44 GLfloat)
    , pos  :: !(V3 GLfloat)
    } deriving Show

initCamera :: Camera
initCamera =
    let pos' = V3 0 0 5
    in
        Camera { view = makeView pos'
               , pos = pos'
               }

moveForward :: Camera -> Camera
moveForward camera =
    let newPos = pos camera - unit
    in if newPos > closest
           then camera { view = makeView newPos
                       , pos = newPos
                       }
           else camera

moveBackward :: Camera -> Camera
moveBackward camera =
    let newPos = pos camera + unit
    in camera { view = makeView newPos
              , pos = newPos
              }

closest :: V3 GLfloat
closest = V3 0 0 1

unit :: V3 GLfloat
unit = V3 0 0 0.05

makeView :: V3 GLfloat -> M44 GLfloat
makeView pos' = lookAt pos' (V3 0 0 0) (V3 0 1 0)
