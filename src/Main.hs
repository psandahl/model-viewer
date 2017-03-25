module Main where

import           Control.Monad    (unless, when)
import           Data.Either      (isLeft)
import           Data.IORef       (IORef, newIORef, readIORef, writeIORef)
import           Data.Maybe       (fromJust, isNothing)
import           Graphics.LWGL    as GL
import           Graphics.LWGL    (ClearBufferMask (..), EnableCapability (..),
                                   FrameBuffer (..))
import           Graphics.UI.GLFW (OpenGLProfile (..), StickyKeysInputMode (..),
                                   VideoMode (..), Window, WindowHint (..))
import qualified Graphics.UI.GLFW as GLFW
import           System.Exit      (exitFailure)

import qualified Backdrop
import           Camera           (Camera (view), initCamera)
import           EventLoop        (eventLoop)
import           Helper           (makeProjection)
import           Input            (initInput)
import qualified Lightning
import           Model            (loadModel, render, renderShadowMap)
import           Options          (Options (..), options)
import           RenderState      (RenderState (..))
import qualified ShadowDebug
import qualified ShadowMap

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
    bailLeft eModel

    eShadowMap <- ShadowMap.init
    bailLeft eShadowMap

    eBackdrop <- Backdrop.init
    bailLeft eBackdrop

    eShadowDebug <- ShadowDebug.init
    bailLeft eShadowDebug

    let Right model' = eModel
        Right shadowMap' = eShadowMap
        Right backdrop' = eBackdrop
        Right shadowDebug' = eShadowDebug
        state =
            RenderState
                { projection = makeProjection width height
                , model = model'
                , shadowMap = shadowMap'
                , backdrop = backdrop'
                , shadowDebug = shadowDebug'
                , camera = initCamera
                , screenWidth = width
                , screenHeight = height
                , timestamp = 0
                , frameDuration = 0
                , mousePosition = Nothing
                , renderWireframe = False
                , lightning = Lightning.init Lightning.defaultPosition
                                             Lightning.whiteColor
                }
    newIORef state

main :: IO ()
main = do
    opts <- options
    (window, width, height) <- createGLContext $ fullscreenMode opts
    GLFW.makeContextCurrent (Just window)
    GLFW.setStickyKeysInputMode window StickyKeysInputMode'Enabled

    ref <- createRenderState (modelFile opts) width height

    GL.glClearColor 0 0 0.4 0
    GL.glEnable DepthTest

    initInput window ref

    eventLoop window $ renderFrame ref

    GLFW.terminate

-- | Render one frame. This one is invoked after events are handled.
renderFrame :: IORef RenderState -> IO ()
renderFrame ref = do
    state <- readIORef ref

    -- Render the model in the shadow map.
    GL.glViewport 0 0 ShadowMap.shadowWidth ShadowMap.shadowHeight
    GL.glBindFramebuffer GLFrameBuffer (ShadowMap.fbo $ shadowMap state)
    GL.glClear [DepthBuffer]

    renderShadowMap (shadowMap state) (lightning state) (model state)

    GL.glBindFramebuffer GLFrameBuffer (FrameBuffer 0)

    -- Render stuff with the current state in the screen buffers.
    GL.glViewport 0 0 (screenWidth state) (screenHeight state)
    GL.glClear [ColorBuffer, DepthBuffer]

    -- Render the model. Render in wireframe if needed.
    if renderWireframe state
        then GL.glPolygonMode FrontAndBack Line
        else GL.glPolygonMode FrontAndBack Fill

    render (projection state) (view $ camera state)
           (lightning state) (model state)

    -- Always reset to fill mode after model rendering.
    GL.glPolygonMode FrontAndBack Fill

    -- Render the backdrop.
    Backdrop.render (projection state) (view $ camera state)
                    (lightning state) (shadowMap state) (backdrop state)

    -- Render the debug display.
    --ShadowDebug.render (projection state) (view $ camera state)
    --                   (ShadowMap.texture $ shadowMap state) (shadowDebug state)

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

bailLeft :: Either String a -> IO ()
bailLeft eValue =
    when (isLeft eValue) $ do
        let Left err = eValue
        putStrLn err
        GLFW.terminate
        exitFailure

defaultWidth :: Int
defaultWidth = 1024

defaultHeight :: Int
defaultHeight = 768
