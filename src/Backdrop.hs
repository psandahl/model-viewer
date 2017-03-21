module Backdrop
    ( Backdrop (..)
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

data Backdrop = Backdrop
    { program      :: !Program
    , mvpLoc       :: !Location
    , shadowMapLoc :: !Location
    , mesh         :: !Mesh
    , backdropPos  :: !(V3 GLfloat)
    } deriving Show

init :: IO (Either String Backdrop)
init = do
    prog <- loadShaders [ (VertexShader, "shaders/backdrop.vert")
                        , (FragmentShader, "shaders/backdrop.frag")
                        ]
    case prog of
        Right prog' -> do
            mesh' <- buildFromList StaticDraw vertices indices'
            mvpLoc' <- GL.glGetUniformLocation prog' "mvp"
            shadowMapLoc' <- GL.glGetUniformLocation prog' "shadowMap"
            return $ Right
                Backdrop
                    { program = prog'
                    , mvpLoc = mvpLoc'
                    , shadowMapLoc = shadowMapLoc'
                    , mesh = mesh'
                    , backdropPos = V3 (-1.5) (-1.2) 0
                    }
        Left err -> return $ Left err

render :: M44 GLfloat -> M44 GLfloat -> Texture -> Backdrop -> IO ()
render proj view texture backdrop = do
    let model = makeTranslate $ backdropPos backdrop
        mvp = proj !*! view !*! model

    GL.glUseProgram (program backdrop)
    GL.glBindVertexArray (vao $ mesh backdrop)
    GL.setMatrix4 (mvpLoc backdrop) mvp

    GL.glActiveTexture (TextureUnit 0)
    GL.glBindTexture Texture2D texture
    GL.glUniform1i (shadowMapLoc backdrop) 0

    GL.drawTrianglesVector (indices $ mesh backdrop)

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
