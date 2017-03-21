module ShadowMap
    ( ShadowMap (..)
    , init
    , shadowWidth
    , shadowHeight
    ) where

import           Foreign       (nullPtr)
import           Graphics.LWGL
import qualified Graphics.LWGL as GL
import           Prelude       hiding (init)

data ShadowMap = ShadowMap
    { fbo     :: !FrameBuffer
    , texture :: !Texture
    }
    deriving Show

init :: IO (Either String ShadowMap)
init = do
    (fbo', texture') <- makeFramebuffer
    return $ Right ShadowMap { fbo = fbo', texture = texture' }

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
