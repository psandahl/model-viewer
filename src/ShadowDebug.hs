module ShadowDebug
    ( ShadowDebug (..)
    , init
    , render
    ) where

import           Graphics.LWGL                   (BufferUsage (..), GLfloat,
                                                  GLuint, Location, Mesh (..),
                                                  Program, ShaderType (..),
                                                  Texture, TextureTarget (..),
                                                  TextureUnit (..),
                                                  VertexArrayObject (..),
                                                  buildFromList, loadShaders)
import qualified Graphics.LWGL                   as GL
import           Graphics.LWGL.Vertex_P_Norm_Tex (Vertex (..))
import           Linear                          (M44, V2 (..), V3 (..), (!*!))
import           Prelude                         hiding (init)

import           Helper                          (makeTranslate)

data ShadowDebug = ShadowDebug
    { program        :: !Program
    , mvpLoc         :: !Location
    , shadowMapLoc   :: !Location
    , mesh           :: !Mesh
    , shadowDebugPos :: !(V3 GLfloat)
    } deriving Show

init :: IO (Either String ShadowDebug)
init = do
    prog <- loadShaders [ (VertexShader, "shaders/shadowdebug.vert")
                        , (FragmentShader, "shaders/shadowdebug.frag")
                        ]
    case prog of
        Right prog' -> do
            mesh' <- buildFromList StaticDraw vertices indices'
            mvpLoc' <- GL.glGetUniformLocation prog' "mvp"
            shadowMapLoc' <- GL.glGetUniformLocation prog' "shadowMap"
            return $ Right
                ShadowDebug
                    { program = prog'
                    , mvpLoc = mvpLoc'
                    , shadowMapLoc = shadowMapLoc'
                    , mesh = mesh'
                    , shadowDebugPos = V3 (-1.5) (-1.2) 0
                    }
        Left err -> return $ Left err

render :: M44 GLfloat -> M44 GLfloat -> Texture -> ShadowDebug -> IO ()
render proj view texture shadowDebug = do
    let model = makeTranslate $ shadowDebugPos shadowDebug
        mvp = proj !*! view !*! model

    GL.glUseProgram (program shadowDebug)
    GL.glBindVertexArray (vao $ mesh shadowDebug)
    GL.setMatrix4 (mvpLoc shadowDebug) mvp

    GL.glActiveTexture (TextureUnit 0)
    GL.glBindTexture Texture2D texture
    GL.glUniform1i (shadowMapLoc shadowDebug) 0

    GL.drawTrianglesVector (indices $ mesh shadowDebug)

    GL.glBindVertexArray (VertexArrayObject 0)


vertices :: [Vertex]
vertices =
    [ Vertex
        { position = V3 1 1 0
        , normal = V3 0 0 1
        , texCoord = V2 1 1
        }
    , Vertex
        { position = V3 (-1) 1 0
        , normal = V3 0 0 1
        , texCoord = V2 0 1
        }
    , Vertex
        { position = V3 (-1) (-1) 0
        , normal = V3 0 0 1
        , texCoord = V2 0 0
        }
    , Vertex
        { position = V3 1 (-1) 0
        , normal = V3 0 0 1
        , texCoord = V2 1 0
        }
    ]

indices' :: [GLuint]
indices' =
    [ 0, 1, 2, 0, 2, 3
    ]
