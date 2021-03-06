module Lightning
    ( Lightning (..)
    , init
    , defaultPosition
    , whiteColor
    ) where

import           Graphics.LWGL (GLfloat)
import           Linear        (M44, V3 (..), lookAt, normalize)
import           Prelude       hiding (init)

data Lightning = Lightning
    { lightDir         :: !(V3 GLfloat)
    , lightPos         :: !(V3 GLfloat)
    , lightView        :: !(M44 GLfloat)
    , lightColor       :: !(V3 GLfloat)
    , ambientStrength  :: !Float
    , specularStrength :: !Float
    , specularShine    :: !Int
    } deriving Show

-- | Initialize lightning parameters. The lightning direction is normalized
-- and already negated to be adapted for lightning calculations.
init :: V3 GLfloat -> V3 GLfloat -> Lightning
init lightPos' lightColor' =
    Lightning
        { lightDir = negate $ lightFromTo lightPos' origo
        , lightPos = lightPos'
        , lightView = lookAt lightPos' origo (V3 0 1 0)
        , lightColor = lightColor'
        , ambientStrength = 0.15
        , specularStrength = 2.0
        , specularShine = 64
        }

-- | The default position of the ligth. In model space the ligth is to the left
-- of the scene. No height, no depth.
defaultPosition :: V3 GLfloat
defaultPosition = V3 (-10) 0 0 -- Have no further away than ten due to shadows.

-- | The color white.
whiteColor :: V3 GLfloat
whiteColor = V3 1 1 1

-- | Calculate the light direction from the starting point to the end point.
lightFromTo :: V3 GLfloat -> V3 GLfloat -> V3 GLfloat
lightFromTo start end = normalize $ end - start

origo :: V3 GLfloat
origo = V3 0 0 0
