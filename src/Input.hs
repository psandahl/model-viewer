module Input
    ( initInput
    ) where

import           Control.Monad    (unless, when)
import           Data.IORef       (IORef, modifyIORef, readIORef, writeIORef)
import           Graphics.LWGL    (GLfloat)
import           Graphics.UI.GLFW (Key (..), KeyState (..), ModifierKeys,
                                   MouseButton (..), MouseButtonState (..),
                                   Window)
import qualified Graphics.UI.GLFW as GLFW
import           Linear           (V2 (..), V3 (..), distance, normalize)

import           Text.Printf      (printf)

import           Camera           (moveBackward, moveForward)
import           Model            (Model (..), rotateLeft, rotateRight)
import           RenderState      (RenderState (..))

initInput :: Window -> IORef RenderState -> IO ()
initInput window ref = do
    GLFW.setScrollCallback window $ Just (scrollCallback ref)
    GLFW.setKeyCallback window $ Just (keyCallback ref)

scrollCallback :: IORef RenderState -> Window -> Double -> Double -> IO ()
scrollCallback ref _window _xoffset yoffset =
    modifyIORef ref $ \state ->
        if yoffset > 0
            then state { camera = moveForward (camera state) }
            else state { camera = moveBackward (camera state) }

keyCallback :: IORef RenderState -> Window -> Key -> Int
            -> KeyState -> ModifierKeys -> IO ()
keyCallback ref _window key _scan keyState _modKeys = do

    -- Left key.
    when (key == Key'Left && activeKey keyState) $
            modifyIORef ref $ \state ->
                state { model = rotateLeft (frameDuration state) (model state) }

    -- Right key.
    when (key == Key'Right && activeKey keyState) $
        modifyIORef ref $ \state ->
            state { model = rotateRight (frameDuration state) (model state) }

activeKey :: KeyState -> Bool
activeKey keyState =
    keyState == KeyState'Pressed || keyState == KeyState'Repeating
