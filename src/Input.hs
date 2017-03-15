module Input
    ( initInput
    ) where

import           Control.Monad    (unless, when)
import           Data.IORef       (IORef, modifyIORef, readIORef, writeIORef)
import           Graphics.LWGL    (GLfloat)
import           Graphics.UI.GLFW (ModifierKeys, MouseButton (..),
                                   MouseButtonState (..), Window)
import qualified Graphics.UI.GLFW as GLFW
import           Linear           (V2 (..), V3 (..), distance, normalize)

import           Text.Printf      (printf)

import           Camera           (moveBackward, moveForward)
import           Model            (Model (..))
import           RenderState      (RenderState (..))

initInput :: Window -> IORef RenderState -> IO ()
initInput window ref = do
    GLFW.setScrollCallback window $ Just (scrollCallback ref)

scrollCallback :: IORef RenderState -> Window -> Double -> Double -> IO ()
scrollCallback ref _window _xoffset yoffset =
    modifyIORef ref $ \state ->
        if yoffset > 0
            then state { camera = moveForward (camera state) }
            else state { camera = moveBackward (camera state) }
