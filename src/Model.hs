module Model
    ( Model (..)
    , loadModel
    , changeRotation
    , render
    ) where

import           Graphics.LWGL (BufferUsage (..), GLfloat, Location, Location,
                                Mesh (..), Program, ShaderType (..), Texture,
                                TextureFormat (..), VertexArrayObject (..))
import qualified Graphics.LWGL as GL
import           Graphics.OBJ
import           Linear        (M44, V3 (..), axisAngle, identity,
                                mkTransformation, normalize, (!*!))

import           ModelSpec     (ModelSpec)
import qualified ModelSpec     as Spec

data Model = Model
    { program :: !Program
    , mvpLoc  :: !Location
    , mesh    :: !Mesh
    , texture :: !(Maybe Texture)
    , bumpMap :: !(Maybe Texture)
    , axis    :: !(V3 GLfloat)
    , angle   :: !GLfloat
    , matrix  :: !(M44 GLfloat)
    } deriving Show

loadModel :: FilePath -> IO (Either String Model)
loadModel file = do
    model <- Spec.fromFile file
    case model of
        Right (Spec.ModelSpec _ (Just _) Nothing) -> do
            let Right model' = model
            es <- expandEithers <$> programFromFile model'
                                <*> meshFromFile model'
                                <*> loadTextureFromFile model'
            case es of
                Right (program', mesh', texture') -> do

                    mvpLoc' <- GL.glGetUniformLocation program' "mvp"

                    GL.glBindVertexArray (VertexArrayObject 0)

                    return $ Right Model
                        { program = program'
                        , mvpLoc = mvpLoc'
                        , mesh = mesh'
                        , texture = Just texture'
                        , bumpMap = Nothing
                        , axis = V3 0 0 0
                        , angle = 0
                        , matrix = identity
                        }

                Left err -> return $ Left err

        Right _ ->
            return $ Left "Not supported"

        Left err -> return $ Left err

expandEithers :: Either String Program
              -> Either String Mesh
              -> Either String Texture
              -> Either String (Program, Mesh, Texture)
expandEithers eProgram eMesh eTexture =
    (,,) <$> eProgram <*> eMesh <*> eTexture

changeRotation :: V3 GLfloat -> GLfloat -> Model -> Model
changeRotation axis' angle' model =
    let newAxis = normalize $ axis' + axis model
        newAngle = angle' + angle model
        newMatrix = makeRotation newAxis newAngle
    in model { axis = newAxis, angle = newAngle, matrix = newMatrix }

render :: M44 GLfloat -> M44 GLfloat -> Model -> IO ()
render projection view model = do
    GL.glUseProgram (program model)
    GL.glBindVertexArray (vao $ mesh model)

    let mvp = projection !*! view !*! (matrix model)
    GL.setMatrix4 (mvpLoc model) mvp
    GL.drawTrianglesVector (indices $ mesh model)

    GL.glBindVertexArray (VertexArrayObject 0)


meshFromFile :: ModelSpec -> IO (Either String Mesh)
meshFromFile model = do
    vectors <- loadVTNFromFile (Spec.file model)
    case vectors of
        Right (vs, is) -> Right <$> GL.buildFromVector StaticDraw vs is
        Left err       -> return $ Left err


programFromFile :: ModelSpec -> IO (Either String Program)
programFromFile spec =
    case (Spec.texture spec, Spec.bumpMap spec) of
        (Just _, Nothing) ->
            GL.loadShaders [ (VertexShader, "shaders/texturedmodel.vert")
                           , (FragmentShader, "shaders/texturedmodel.frag")
                           ]
        _                 -> return $ Left "Not supported yet"

loadTextureFromFile :: ModelSpec -> IO (Either String Texture)
loadTextureFromFile spec =
    case Spec.texture spec of
        Just f  -> GL.loadTexture2D RGB8 True f
        Nothing -> return $ Left "No file specified"

makeRotation :: V3 GLfloat -> GLfloat -> M44 GLfloat
makeRotation axis' angle' =
    mkTransformation (axisAngle axis' angle') (V3 0 0 0)

{-loadBumpMapFromFile :: ModelSpec -> IO (Either String Texture)
loadBumpMapFromFile spec =
    case Spec.bumpMap spec of
        Just f  -> GL.loadTexture2D RGB8 False f
        Nothing -> return $ Left "No file specified"
-}
