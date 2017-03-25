{-# LANGUAGE MultiWayIf #-}
module Camera
    ( Camera (view)
    , initCamera
    , moveForward
    , moveBackward
    , moveCamera
    ) where

import           Graphics.LWGL (GLfloat)
import           Linear        (M44, V3 (..), lookAt)

data Camera = Camera
    { view     :: !(M44 GLfloat)
    , angle    :: !GLfloat
    , distance :: !GLfloat
    , height   :: !GLfloat
    } deriving Show

-- | Init a camera, with a default position of five units back. No depth,
-- no height.
initCamera :: Camera
initCamera =
    let angle' = 0
        distance' = 15
        height' = 0
    in
        Camera { view = makeView $ makeCameraPosition angle' distance' height'
               , angle = angle'
               , distance = distance'
               , height = height'
               }

moveForward :: Camera -> Camera
moveForward camera =
    let newDistance = distance camera - 0.5
    in if newDistance > 1.0
           then camera { distance = newDistance
                       , view = makeView $ makeCameraPosition (angle camera)
                                                              newDistance
                                                              (height camera)
                       }
           else camera

moveBackward :: Camera -> Camera
moveBackward camera =
    let newDistance = distance camera + 0.5
    in camera { distance = newDistance
              , view = makeView $ makeCameraPosition (angle camera)
                                                     newDistance
                                                     (height camera)
              }

moveCamera :: Double -> Double -> Camera -> Camera
moveCamera xDiff yDiff camera =
    let newAngle = angle camera + diffToAngle xDiff
        newHeight = diffToHeight (height camera) yDiff
    in camera { angle = newAngle
              , height = newHeight
              , view = makeView $ makeCameraPosition newAngle
                                                     (distance camera)
                                                     newHeight
              }

diffToAngle :: Double -> GLfloat
diffToAngle xDiff = realToFrac $ 0.01 * xDiff

diffToHeight :: GLfloat -> Double -> GLfloat
diffToHeight height' yDiff =
    let newHeight = height' + realToFrac (0.1 * yDiff)
    in if | newHeight > maxHeight -> maxHeight
          | newHeight < minHeight -> minHeight
          | otherwise -> newHeight
    where
        maxHeight :: GLfloat
        maxHeight = 100

        minHeight :: GLfloat
        minHeight = -100

makeView :: V3 GLfloat -> M44 GLfloat
makeView pos' = lookAt pos' (V3 0 0 0) (V3 0 1 0)

makeCameraPosition :: GLfloat -> GLfloat -> GLfloat -> V3 GLfloat
makeCameraPosition angle' distance' height' =
    let (V3 x _ z) = unitAtAngle angle'
    in V3 (distance' * x) height' (distance' * z)

-- | Unit vector, with no height/y value, but with a given angle. At angle zero
-- the vector points at x = 0, z = 1.
unitAtAngle :: GLfloat -> V3 GLfloat
unitAtAngle angle' = V3 (sin angle') 0 (cos angle')
