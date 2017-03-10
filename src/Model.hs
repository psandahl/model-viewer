module Model
    ( Model (..)
    , loadModel
    , render
    ) where

import           Data.Vector.Storable            (Vector)
import           Graphics.LWGL                   (BufferUsage (..), GLfloat,
                                                  GLuint, Mesh (..), Program,
                                                  ShaderType (..), Texture,
                                                  TextureFormat (..))
import qualified Graphics.LWGL                   as GL
import qualified Graphics.LWGL.Vertex_P_Norm_Tex as VTN
import           Graphics.OBJ
import           Linear                          (M44)

import           ModelSpec                       (ModelSpec)
import qualified ModelSpec                       as Spec

data Model = Model
    { program :: !Program
    , mesh    :: !Mesh
    , texture :: !(Maybe Texture)
    , bumpMap :: !(Maybe Texture)
    } deriving Show

loadModel :: FilePath -> IO (Either String Model)
loadModel = undefined

render :: M44 GLfloat -> M44 GLfloat -> Model -> IO ()
render = undefined

meshFromFile :: ModelSpec -> IO (Either String Mesh)
meshFromFile model = do
    vectors <- loadVTNFromFile (Spec.file model)
    case vectors of
        Right (vs, is) -> Right <$> GL.buildFromVector StaticDraw vs is
        Left err       -> return $ Left err


programFromFile :: ModelSpec -> IO (Either String Program)
programFromFile spec =
    case (Spec.texture spec, Spec.bumpMap spec) of
        (Just _, Nothing) -> GL.loadShaders [ (VertexShader, "shaders/fullmodel.vert")
                                            , (FragmentShader, "shaders/fullmodel.frag")
                                            ]
        _                 -> return $ Left "Not supported yet"

loadTextureFromFile :: ModelSpec -> IO (Either String Texture)
loadTextureFromFile spec =
    case Spec.texture spec of
        Just f  -> GL.loadTexture2D RGB8 True f
        Nothing -> return $ Left "No file specified"

loadBumpMapFromFile :: ModelSpec -> IO (Either String Texture)
loadBumpMapFromFile spec =
    case Spec.bumpMap spec of
        Just f  -> GL.loadTexture2D RGB8 False f
        Nothing -> return $ Left "No file specified"
