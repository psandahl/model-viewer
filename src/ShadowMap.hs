module ShadowMap
    ( ShadowMap (..)
    , init
    , shadowWidth
    , shadowHeight
    ) where

import           Foreign       (nullPtr)
import           Graphics.LWGL (DrawBufferMode (..), FrameBuffer (..),
                                FrameBufferAttachment (..),
                                FrameBufferTarget (..), GLfloat,
                                ImageComponentCount (..), Location,
                                PixelFormat (..), PixelType (..), Program,
                                ShaderType (..), Texture,
                                TextureParameterName (..),
                                TextureParameterValue (..), TextureTarget (..))
import qualified Graphics.LWGL as GL
import           Linear
import           Prelude       hiding (init)

data ShadowMap = ShadowMap
    { program    :: !Program
    , mvpLoc     :: !Location
    , projection :: !(M44 GLfloat)
    , fbo        :: !FrameBuffer
    , texture    :: !Texture
    }
    deriving Show

init :: IO (Either String ShadowMap)
init = do
    prog <- GL.loadShaders [ (VertexShader, "shaders/shadowmap.vert")
                           , (FragmentShader, "shaders/shadowmap.frag")
                           ]
    case prog of
        Right prog' -> do
            (fbo', texture') <- makeFramebuffer
            mvpLoc' <- GL.glGetUniformLocation prog' "mvp"
            return $ Right
                ShadowMap
                    { program = prog'
                    , mvpLoc = mvpLoc'
                    , projection = ortho (-10) 10 (-10) 10 0.1 65
                    , fbo = fbo'
                    , texture = texture'
                    }

        Left err -> return $ Left err

shadowWidth :: Int
shadowWidth = 1024

shadowHeight :: Int
shadowHeight = 1024

makeFramebuffer :: IO (FrameBuffer, Texture)
makeFramebuffer = do
    [fbo'] <- GL.glGenFramebuffers 1
    [tex] <- GL.glGenTextures 1

    GL.glBindTexture Texture2D tex
    GL.glTexImage2D Texture2D 0 ImgDepthComponent
                    (fromIntegral shadowWidth) (fromIntegral shadowHeight)
                    PxlDepthComponent PxlFloat nullPtr
    GL.glTexParameteri Texture2D TextureMinFilter GLNearest
    GL.glTexParameteri Texture2D TextureMagFilter GLNearest
    GL.glTexParameteri Texture2D TextureWrapS GLClampToEdge
    GL.glTexParameteri Texture2D TextureWrapT GLClampToEdge

    GL.glBindFramebuffer GLFrameBuffer fbo'
    GL.glFramebufferTexture2D GLFrameBuffer GLDepthAttachment Texture2D tex 0
    GL.glDrawBuffer DrawNone
    GL.glBindFramebuffer GLFrameBuffer (FrameBuffer 0)

    return (fbo', tex)
