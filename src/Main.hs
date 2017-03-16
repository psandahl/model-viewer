module Main where

import           Control.Monad    (unless, when)
import           Data.Either      (isLeft)
import           Data.IORef       (IORef, newIORef, readIORef, writeIORef)
import           Data.Maybe       (fromJust, isNothing)
import           Graphics.LWGL    as GL
import           Graphics.LWGL    (ClearBufferMask (..), EnableCapability (..))
import           Graphics.UI.GLFW (OpenGLProfile (..), StickyKeysInputMode (..),
                                   VideoMode (..), Window, WindowHint (..))
import qualified Graphics.UI.GLFW as GLFW
import           System.Exit      (exitFailure)

import           Camera           (Camera (view), initCamera)
import           EventLoop        (eventLoop)
import           Helper           (makeProjection)
import           Input            (initInput)
import           Model            (loadModel, render)
import           RenderState      (RenderState (..))

createGLContext :: Bool -> IO (Window, Int, Int)
createGLContext fullscreen = do
    initSuccess <- GLFW.init
    unless initSuccess $ do
        putStrLn "GLFW initialization failed"
        exitFailure

    GLFW.windowHint $ WindowHint'Resizable True
    GLFW.windowHint $ WindowHint'Samples 4
    GLFW.windowHint $ WindowHint'ContextVersionMajor 3
    GLFW.windowHint $ WindowHint'ContextVersionMinor 3
    GLFW.windowHint $ WindowHint'OpenGLForwardCompat True
    GLFW.windowHint $ WindowHint'OpenGLProfile OpenGLProfile'Core

    window <- if fullscreen then makeFullscreen else makeWindow
    when (isNothing window) $ do
        putStrLn "Failed to create GLFW window"
        GLFW.terminate
        exitFailure

    return $ fromJust window

createRenderState :: FilePath -> Int -> Int -> IO (IORef RenderState)
createRenderState file width height = do
    eModel <- loadModel file
    when (isLeft eModel) $ do
        let Left err = eModel
        putStrLn err
        GLFW.terminate
        exitFailure

    let Right model' = eModel
        state =
            RenderState
                { projection = makeProjection width height
                , camera = initCamera
                , model = model'
                , timestamp = 0
                , frameDuration = 0
                , mousePosition = Nothing
                , renderWireframe = False
                }
    newIORef state

main :: IO ()
main = do
    (window, width, height) <- createGLContext False
    GLFW.makeContextCurrent (Just window)
    GLFW.setStickyKeysInputMode window StickyKeysInputMode'Enabled

    ref <- createRenderState "example-files/brickcube.json" width height

    GL.glViewport 0 0 width height
    GL.glClearColor 1 1 1 0
    GL.glEnable DepthTest
    GL.glPolygonMode FrontAndBack Line

    initInput window ref

    eventLoop window $ renderFrame ref

    GLFW.terminate

-- | Render one frame. This one is invoked after events are handled.
renderFrame :: IORef RenderState -> IO ()
renderFrame ref = do
    state <- readIORef ref

    if renderWireframe state
        then GL.glPolygonMode FrontAndBack Line
        else GL.glPolygonMode FrontAndBack Fill

    -- Render stuff with the current state.
    GL.glClear [ColorBuffer, DepthBuffer]
    render (projection state) (view $ camera state) (model state)

    -- Update the timestamp and duration for the state.
    mTime <- GLFW.getTime
    case mTime of
        Just time -> do
            let state' = state { frameDuration = time - timestamp state
                               , timestamp = time
                               }
            writeIORef ref state'
        Nothing -> putStrLn "Error: Cannot read time"

makeWindow :: IO (Maybe (Window, Int, Int))
makeWindow = do
    window <- GLFW.createWindow defaultWidth defaultHeight "Model Viewer" Nothing Nothing
    case window of
        Just window' -> return $ Just (window', defaultWidth, defaultHeight)
        Nothing      -> return Nothing

makeFullscreen :: IO (Maybe (Window, Int, Int))
makeFullscreen = do
    monitor <- GLFW.getPrimaryMonitor
    case monitor of
        Just monitor' -> do
            mode <- GLFW.getVideoMode monitor'
            case mode of
                Just mode' -> do
                    let w = videoModeWidth mode'
                        h = videoModeHeight mode'
                    window <- GLFW.createWindow w h "ModelViewer" (Just monitor') Nothing
                    case window of
                        Just window' -> return $ Just (window', w, h)
                        Nothing      -> return Nothing

                Nothing -> return Nothing

        Nothing -> return Nothing

defaultWidth :: Int
defaultWidth = 1024

defaultHeight :: Int
defaultHeight = 768
