module Helper
    ( makeProjection
    , degToRad
    ) where

import           Graphics.LWGL (GLfloat)
import           Linear        (M44, perspective)

makeProjection :: Int -> Int -> M44 GLfloat
makeProjection width height =
    perspective (degToRad 45)
                (fromIntegral width / fromIntegral height)
                0.001 10000

degToRad :: Floating a => a -> a
degToRad deg = deg * (pi / 180.0)
