module Logo
    ( Logo (..)
    , init
    , render
    ) where

import           Graphics.LWGL              (BufferUsage (..), GLfloat, GLuint,
                                             Location, Mesh (..), Program (..),
                                             ShaderType (..),
                                             VertexArrayObject (..))
import qualified Graphics.LWGL              as GL
import           Graphics.LWGL.Vertex_P_Tex (Vertex (..))
import           Linear                     (M44, V2 (..), V3 (..), V4 (..),
                                             (!*!))
import           Prelude                    hiding (init)

data Logo = Logo
    { program  :: !Program
    , modelLoc :: !Location
    , mesh     :: !Mesh
    , model    :: !(M44 GLfloat)
    } deriving Show

init :: IO (Either String Logo)
init = do
    prog <- GL.loadShaders [ (VertexShader, "shaders/logo.vert")
                           , (FragmentShader, "shaders/logo.frag")
                           ]
    case prog of
        Right prog' -> do

            mesh' <- GL.buildFromList StaticDraw vertices indices'
            modelLoc' <- GL.glGetUniformLocation prog' "model"

            return $ Right Logo
                { program = prog'
                , modelLoc = modelLoc'
                , mesh = mesh'
                , model = mkTrans (-0.75) 0.75 !*! mkScale 0.25 0.25
                }

        Left err -> return $ Left err

render :: Logo -> IO ()
render logo = do
    GL.glUseProgram $ program logo
    GL.glBindVertexArray (vao $ mesh logo)

    GL.setMatrix4 (modelLoc logo) (model logo)
    GL.drawTrianglesVector (indices $ mesh logo)

    GL.glBindVertexArray (VertexArrayObject 0)
    GL.glUseProgram (Program 0)

vertices :: [Vertex]
vertices =
    [ Vertex
        { position = V3 1 1 0
        , texCoord = V2 1 1
        }
    , Vertex
        { position = V3 (-1) 1 0
        , texCoord = V2 0 1
        }
    , Vertex
        { position = V3 (-1) (-1) 0
        , texCoord = V2 0 0
        }
    , Vertex
        { position = V3 1 (-1) 0
        , texCoord = V2 1 0
        }
    ]

indices' :: [GLuint]
indices' =
    [ 0, 1, 2
    , 0, 2, 3
    ]

mkScale :: GLfloat -> GLfloat -> M44 GLfloat
mkScale x y =
    V4 (V4 x 0 0 0) (V4 0 y 0 0) (V4 0 0 1 0) (V4 0 0 0 1)

mkTrans :: GLfloat -> GLfloat -> M44 GLfloat
mkTrans x y =
    V4 (V4 1 0 0 x) (V4 0 1 0 y) (V4 0 0 1 0) (V4 0 0 0 1)
