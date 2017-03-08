module Main where

import           Control.Monad    (unless, when)
import           Data.Maybe       (fromJust, isNothing)
import           Graphics.LWGL    as GL
import           Graphics.LWGL    (ClearBufferMask (..), EnableCapability (..))
import           Graphics.UI.GLFW (OpenGLProfile (..), StickyKeysInputMode (..),
                                   Window, WindowHint (..))
import qualified Graphics.UI.GLFW as GLFW
import           System.Exit      (exitFailure)

import           EventLoop        (eventLoop)

createGLContext :: IO Window
createGLContext = do
    initSuccess <- GLFW.init
    unless initSuccess $ do
        putStrLn "GLFW initialization failed"
        exitFailure

    GLFW.windowHint $ WindowHint'Resizable False
    GLFW.windowHint $ WindowHint'Samples 4
    GLFW.windowHint $ WindowHint'ContextVersionMajor 3
    GLFW.windowHint $ WindowHint'ContextVersionMinor 3
    GLFW.windowHint $ WindowHint'OpenGLForwardCompat True
    GLFW.windowHint $ WindowHint'OpenGLProfile OpenGLProfile'Core

    window <- GLFW.createWindow width height "Model Viewer" Nothing Nothing
    when (isNothing window) $ do
        putStrLn "Failed to create GLFW window"
        GLFW.terminate
        exitFailure

    return $ fromJust window

main :: IO ()
main = do
    window <- createGLContext
    GLFW.makeContextCurrent (Just window)
    GLFW.setStickyKeysInputMode window StickyKeysInputMode'Enabled

    GL.glClearColor 1 1 1 1
    GL.glEnable DepthTest

    eventLoop window renderScene

    GLFW.terminate

renderScene :: IO ()
renderScene =
    GL.glClear [ColorBuffer, DepthBuffer]

width :: Int
width = 1280

height :: Int
height = 960
