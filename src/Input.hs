module Input
    ( initInput
    ) where

import           Data.IORef       (IORef)
import           Graphics.UI.GLFW (Window)

import           RenderState      (RenderState (..))

initInput :: Window -> IORef RenderState -> IO ()
initInput = undefined
