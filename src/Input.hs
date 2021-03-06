module Input
    ( initInput
    ) where

import           Control.Monad    (when)
import           Data.IORef       (IORef, modifyIORef, readIORef, writeIORef)
import qualified Graphics.LWGL    as GL
import           Graphics.UI.GLFW (Key (..), KeyState (..), ModifierKeys,
                                   MouseButton (..), MouseButtonState (..),
                                   Window)
import qualified Graphics.UI.GLFW as GLFW

import           Camera           (moveBackward, moveCamera, moveForward)
import           Helper           (makeProjection)
import           Model            (rotateLeft, rotateRight)
import           RenderState      (RenderState (..))

initInput :: Window -> IORef RenderState -> IO ()
initInput window ref = do
    GLFW.setScrollCallback window $ Just (scrollCallback ref)
    GLFW.setKeyCallback window $ Just (keyCallback ref)
    GLFW.setMouseButtonCallback window $ Just (mouseButtonCallback ref)
    GLFW.setCursorPosCallback window $ Just (cursorPosCallback ref)
    GLFW.setWindowSizeCallback window $ Just (windowSizeCallback ref)

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

    -- Toggle wireframe rendering.
    when (key == Key'R && keyState == KeyState'Pressed) $
        modifyIORef ref $ \state ->
            state { renderWireframe = not $ renderWireframe state }

activeKey :: KeyState -> Bool
activeKey keyState =
    keyState == KeyState'Pressed || keyState == KeyState'Repeating

mouseButtonCallback :: IORef RenderState -> Window -> MouseButton
                    -> MouseButtonState -> ModifierKeys -> IO ()
mouseButtonCallback ref window button buttonState _modKeys = do
    -- Press left mouse button.
    when (button == MouseButton'1 && buttonState == MouseButtonState'Pressed) $ do
        pos <- GLFW.getCursorPos window
        modifyIORef ref $ \state ->
            state { mousePosition = Just pos }

    -- Release left mouse button.
    when (button == MouseButton'1 && buttonState == MouseButtonState'Released) $
        modifyIORef ref $ \state ->
            state { mousePosition = Nothing }

cursorPosCallback :: IORef RenderState -> Window -> Double -> Double -> IO ()
cursorPosCallback ref _window x y = do
    state <- readIORef ref
    case mousePosition state of
        Just (oldX, oldY) -> do
            let xDiff = x - oldX
                yDiff = y - oldY
            writeIORef ref $ state { mousePosition = Just (x, y)
                                   , camera = moveCamera xDiff yDiff (camera state)
                                   }
        Nothing           -> return ()

windowSizeCallback :: IORef RenderState -> Window -> Int -> Int -> IO ()
windowSizeCallback ref _window width height = do
    GL.glViewport 0 0 width height
    modifyIORef ref $ \state ->
        state { projection = makeProjection width height
              , screenWidth = width
              , screenHeight = height
              }
