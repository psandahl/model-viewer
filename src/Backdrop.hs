module Backdrop
    ( Backdrop (..)
    , init
    , render
    ) where

import           Graphics.LWGL               (BufferUsage (..), GLfloat, GLuint,
                                              Location, Mesh (..), Program,
                                              ShaderType (..),
                                              VertexArrayObject (..),
                                              buildFromList, loadShaders)
import qualified Graphics.LWGL               as GL
import           Graphics.LWGL.Vertex_P_Norm (Vertex (..))
import           Linear                      (M44, V3 (..), m33_to_m44, scaled,
                                              (!*!))
import           Prelude                     hiding (init)

data Backdrop = Backdrop
    { program  :: !Program
    , mvpLoc   :: !Location
    , modelLoc :: !Location
    , model    :: !(M44 GLfloat)
    , mesh     :: !Mesh
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
            modelLoc' <- GL.glGetUniformLocation prog' "model"
            return $ Right
                Backdrop
                    { program = prog'
                    , mvpLoc = mvpLoc'
                    , modelLoc = modelLoc'
                    , model = m33_to_m44 $ scaled (scaleVector 5)
                    , mesh = mesh'
                    }

        Left err -> return $ Left err

render :: M44 GLfloat -> M44 GLfloat -> Backdrop -> IO ()
render proj view backdrop = do
    let model' = model backdrop
        mvp = proj !*! view !*! model'

    GL.glUseProgram (program backdrop)
    GL.glBindVertexArray (vao $ mesh backdrop)
    GL.setMatrix4 (mvpLoc backdrop) mvp
    GL.setMatrix4 (modelLoc backdrop) model'

    GL.drawTrianglesVector (indices $ mesh backdrop)

    GL.glBindVertexArray (VertexArrayObject 0)

scaleVector :: GLfloat -> V3 GLfloat
scaleVector s = V3 s s s

vertices :: [Vertex]
vertices =
    [ -- Vertices for the 'right' wall.
      Vertex
        { position = V3 1 1 1
        , normal = V3 (-1) 0 0
        }
    , Vertex
        { position = V3 1 1 (-1)
        , normal = V3 (-1) 0 0
        }
    , Vertex
        { position = V3 1 (-1) (-1)
        , normal = V3 (-1) 0 0
        }
    , Vertex
        { position = V3 1 (-1) 1
        , normal = V3 (-1) 0 0
        }
    ]

indices' :: [GLuint]
indices' =
    [ 0, 1, 2, 0, 2, 3 -- Right face
    ]
