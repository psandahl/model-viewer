module Input
    ( initInput
    ) where

import           Data.IORef       (IORef, modifyIORef)
import           Graphics.UI.GLFW (Window)
import qualified Graphics.UI.GLFW as GLFW

import           Camera           (moveBackward, moveForward)
import           RenderState      (RenderState (..))

initInput :: Window -> IORef RenderState -> IO ()
initInput window ref = GLFW.setScrollCallback window $ Just (scrollCallback ref)

scrollCallback :: IORef RenderState -> Window -> Double -> Double -> IO ()
scrollCallback ref _window _xoffset yoffset =
    modifyIORef ref $ \state ->
        if yoffset > 0
            then state { camera = moveForward (camera state) }
            else state { camera = moveBackward (camera state) }
