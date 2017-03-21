module RenderState
    ( RenderState (..)
    ) where

import           Graphics.LWGL (GLfloat)
import           Linear        (M44)

import           Backdrop      (Backdrop)
import           Camera        (Camera)
import           Lightning     (Lightning)
import           Model         (Model)

data RenderState = RenderState
    { projection      :: !(M44 GLfloat)
    , model           :: !Model
    , backdrop        :: !Backdrop
    , camera          :: !Camera
    , screenWidth     :: !Int
    , screenHeight    :: !Int
    , timestamp       :: !Double
    , frameDuration   :: !Double
    , mousePosition   :: !(Maybe (Double, Double))
    , renderWireframe :: !Bool
    , lightning       :: !Lightning
    } deriving Show
