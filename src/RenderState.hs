module RenderState
    ( RenderState (..)
    ) where

import           Graphics.LWGL (GLfloat)
import           Linear        (M44)

import           Camera        (Camera)
import           Model         (Model)

data RenderState = RenderState
    { projection :: !(M44 GLfloat)
    , model      :: !Model
    , camera     :: !Camera
    , mouseDown  :: !Bool
    } deriving Show
