module Backdrop
    ( Backdrop (..)
    , init
    , render
    ) where

import           Graphics.LWGL               (BufferUsage (..), GLfloat, GLuint,
                                              Location, Mesh (..), Program,
                                              ShaderType (..),
                                              TextureTarget (..),
                                              TextureUnit (..),
                                              VertexArrayObject (..),
                                              buildFromList, loadShaders)
import qualified Graphics.LWGL               as GL
import           Graphics.LWGL.Vertex_P_Norm (Vertex (..))
import           Linear                      (M44, V3 (..), m33_to_m44, scaled,
                                              (!*!))
import           Prelude                     hiding (init)

import           Lightning                   (Lightning (..))
import           ShadowMap                   (ShadowMap (projection, texture))

data Backdrop = Backdrop
    { program             :: !Program
    , mvpLoc              :: !Location
    , viewLoc             :: !Location
    , modelLoc            :: !Location
    , lightVPLoc          :: !Location
    , lightDirLoc         :: !Location
    , lightColorLoc       :: !Location
    , ambientStrengthLoc  :: !Location
    , specularStrengthLoc :: !Location
    , shadowMapLoc        :: !Location
    , model               :: !(M44 GLfloat)
    , mesh                :: !Mesh
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
            viewLoc' <- GL.glGetUniformLocation prog' "view"
            modelLoc' <- GL.glGetUniformLocation prog' "model"
            lightVPLoc' <- GL.glGetUniformLocation prog' "lightVP"
            lightDirLoc' <- GL.glGetUniformLocation prog' "lightDir"
            lightColorLoc' <- GL.glGetUniformLocation prog' "lightColor"
            ambientStrengthLoc' <- GL.glGetUniformLocation prog' "ambientStrength"
            specularStrengthLoc' <- GL.glGetUniformLocation prog' "specularStrength"
            shadowMapLoc' <- GL.glGetUniformLocation prog' "shadowMap"
            return $ Right
                Backdrop
                    { program = prog'
                    , mvpLoc = mvpLoc'
                    , viewLoc = viewLoc'
                    , modelLoc = modelLoc'
                    , lightVPLoc = lightVPLoc'
                    , lightDirLoc = lightDirLoc'
                    , lightColorLoc = lightColorLoc'
                    , ambientStrengthLoc = ambientStrengthLoc'
                    , specularStrengthLoc = specularStrengthLoc'
                    , shadowMapLoc = shadowMapLoc'
                    , model = m33_to_m44 $ scaled (scaleVector 20)
                    , mesh = mesh'
                    }

        Left err -> return $ Left err

render :: M44 GLfloat -> M44 GLfloat -> Lightning -> ShadowMap -> Backdrop -> IO ()
render proj view lightning shadowMap backdrop = do
    let model' = model backdrop
        mvp = proj !*! view !*! model'
        lightVP = projection shadowMap !*! lightView lightning

    GL.glUseProgram (program backdrop)
    GL.glBindVertexArray (vao $ mesh backdrop)
    GL.setMatrix4 (mvpLoc backdrop) mvp
    GL.setMatrix4 (viewLoc backdrop) view
    GL.setMatrix4 (modelLoc backdrop) model'
    GL.setMatrix4 (lightVPLoc backdrop) lightVP
    GL.setVector3 (lightDirLoc backdrop) (lightDir lightning)
    GL.setVector3 (lightColorLoc backdrop) (lightColor lightning)
    GL.glUniform1f (ambientStrengthLoc backdrop) (ambientStrength lightning)
    GL.glUniform1f (specularStrengthLoc backdrop) (specularStrength lightning)

    GL.glActiveTexture (TextureUnit 0)
    GL.glBindTexture Texture2D (texture shadowMap)
    GL.glUniform1i (shadowMapLoc backdrop) 0

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
    -- Vertices for the 'back' wall. Start at index 4.
    , Vertex
        { position = V3 1 1 (-1)
        , normal = V3 0 0 1
        }
    , Vertex
        { position = V3 (-1) 1 (-1)
        , normal = V3 0 0 1
        }
    , Vertex
        { position = V3 (-1) (-1) (-1)
        , normal = V3 0 0 1
        }
    , Vertex
        { position = V3 1 (-1) (-1)
        , normal = V3 0 0 1
        }
    ]

indices' :: [GLuint]
indices' =
    [ 0, 1, 2, 0, 2, 3 -- Right wall
    , 4, 5, 6, 4, 6, 7 -- Back wall
    ]
