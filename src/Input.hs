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
import           Model            (Model (..), changeRotation)
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
cursorPosCallback ref _win x y = do
    state <- readIORef ref
    when (mouseDown state) $ do
        let x' = realToFrac x
            y' = realToFrac y
        case mousePos state of
            Just (oldX, oldY) -> do
                let oldPos = V2 oldX oldY
                    newPos = V2 x' y'
                    change = 0.05 * axisChange oldPos newPos
                    --change = 0.01 * normalize (V3 1 1 0)
                print "---"
                print change
                print (axis $ model state)
                writeIORef ref state { model = changeRotation change 0.001 $ model state
                                     , mousePos = Just (x', y')
                                     }
                print (axis $ changeRotation change 0.01 $ model state)
            Nothing -> writeIORef ref state { mousePos = Just (x', y')}

    -- Button not pressed. Just force Nothing to position.
    unless (mouseDown state) $
        writeIORef ref state { mousePos = Nothing }

axisChange :: V2 GLfloat -> V2 GLfloat -> V3 GLfloat
axisChange oldPos newPos =
    let (V2 x y) = newPos - oldPos
    in normalize $ V3 y x 0
