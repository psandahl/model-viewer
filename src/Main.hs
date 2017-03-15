module Main where

import           Control.Monad    (unless, when)
import           Data.Either      (isLeft)
import           Data.IORef       (IORef, newIORef, readIORef, writeIORef)
import           Data.Maybe       (fromJust, isNothing)
import           Graphics.LWGL    as GL
import           Graphics.LWGL    (ClearBufferMask (..), EnableCapability (..),
                                   GLfloat)
import           Graphics.UI.GLFW (OpenGLProfile (..), StickyKeysInputMode (..),
                                   Window, WindowHint (..))
import qualified Graphics.UI.GLFW as GLFW
import           Linear
import           System.Exit      (exitFailure)

import           Camera           (Camera (view), initCamera)
import           EventLoop        (eventLoop)
import           Input            (initInput)
import           Model            (Model, loadModel, render)
import           RenderState      (RenderState (..))

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

createRenderState :: FilePath -> IO (IORef RenderState)
createRenderState file = do
    eModel <- loadModel file
    when (isLeft eModel) $ do
        let Left err = eModel
        putStrLn err
        GLFW.terminate
        exitFailure

    let Right model' = eModel
        state =
            RenderState
                { projection = makeProjection
                , camera = initCamera
                , model = model'
                }
    newIORef state

main :: IO ()
main = do
    window <- createGLContext
    GLFW.makeContextCurrent (Just window)
    GLFW.setStickyKeysInputMode window StickyKeysInputMode'Enabled

    ref <- createRenderState "example-files/brickcube.json"

    GL.glClearColor 1 1 1 0
    GL.glEnable DepthTest
    GL.glPolygonMode FrontAndBack Line

    initInput window ref

    eventLoop window $ renderScene ref

    GLFW.terminate

renderScene :: IORef RenderState -> IO ()
renderScene ref = do
    state <- readIORef ref

    GL.glClear [ColorBuffer, DepthBuffer]
    render (projection state) (view $ camera state) (model state)

width :: Int
width = 1280

height :: Int
height = 960

makeProjection :: M44 GLfloat
makeProjection =
    perspective (degToRad 45)
                (fromIntegral width / fromIntegral height)
                0.001 10000

makeView :: V3 GLfloat -> M44 GLfloat
makeView pos = lookAt pos (V3 0 0 0) (V3 0 1 0)

degToRad :: Floating a => a -> a
degToRad deg = deg * (pi / 180.0)
