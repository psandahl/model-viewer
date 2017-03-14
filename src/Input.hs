module Input
    ( initInput
    ) where

import           Control.Monad    (when)
import           Data.IORef       (IORef, modifyIORef)
import           Graphics.LWGL    (GLfloat)
import           Graphics.UI.GLFW (ModifierKeys, MouseButton (..),
                                   MouseButtonState (..), Window)
import qualified Graphics.UI.GLFW as GLFW
import           Linear           (V2 (..), V3 (..), distance, normalize)

import           Text.Printf      (printf)

import           Camera           (moveBackward, moveForward)
import           RenderState      (RenderState (..))

initInput :: Window -> IORef RenderState -> IO ()
initInput window ref = do
    GLFW.setScrollCallback window $ Just (scrollCallback ref)
    GLFW.setMouseButtonCallback window $ Just (mouseButtonCallback ref)
    GLFW.setCursorPosCallback window $ Just (cursorPosCallback ref)

scrollCallback :: IORef RenderState -> Window -> Double -> Double -> IO ()
scrollCallback ref _window _xoffset yoffset =
    modifyIORef ref $ \state ->
        if yoffset > 0
            then state { camera = moveForward (camera state) }
            else state { camera = moveBackward (camera state) }

mouseButtonCallback :: IORef RenderState
                    -> Window
                    -> MouseButton
                    -> MouseButtonState
                    -> ModifierKeys
                    -> IO ()
mouseButtonCallback ref _window button buttonState _keys =
    when (button == MouseButton'1) $
        modifyIORef ref $ \state ->
            state { mouseDown = buttonState == MouseButtonState'Pressed }

cursorPosCallback :: IORef RenderState -> Window -> Double -> Double -> IO ()
cursorPosCallback _ref _win x y =
    printf "X=%f, Y=%f\n" x y

axisChange :: V2 GLfloat -> V2 GLfloat -> V3 GLfloat
axisChange oldPos newPos =
    let (V2 x y) = newPos - oldPos
    in normalize $ V3 (abs y) (abs x) 0
