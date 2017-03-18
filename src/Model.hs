module Model
    ( Model (..)
    , loadModel
    , rotateLeft
    , rotateRight
    , render
    ) where

import           Graphics.LWGL (BufferUsage (..), GLfloat, Location, Location,
                                Mesh (..), Program, ShaderType (..), Texture,
                                TextureFormat (..), VertexArrayObject (..))
import qualified Graphics.LWGL as GL
import           Graphics.OBJ  (ObjData (..), loadObjFromFile)
import           Linear        (M44, V3 (..), axisAngle, mkTransformation,
                                (!*!))

import           Lightning     (Lightning (..))
import           ModelSpec     (ModelSpec)
import qualified ModelSpec     as Spec

data Model = Model
    { program             :: !Program
    , mvpLoc              :: !Location
    , viewLoc             :: !Location
    , modelLoc            :: !Location
    , lightDirLoc         :: !Location
    , lightColorLoc       :: !Location
    , ambientStrengthLoc  :: !Location
    , specularStrengthLoc :: !Location
    , specularShineLoc    :: !Location
    , mesh                :: !Mesh
    , texture             :: !(Maybe Texture)
    , bumpMap             :: !(Maybe Texture)
    , angle               :: !GLfloat
    , matrix              :: !(M44 GLfloat)
    } deriving Show

loadModel :: FilePath -> IO (Either String Model)
loadModel file = do
    spec <- Spec.fromFile file
    case spec of
        Right spec' -> do
            resources <- loadFileResources spec'
            case resources of
                Right resources' -> do
                    model' <- finalizeModel resources'
                    return $ Right model'

                Left err -> return $ Left err

        Left err -> return $ Left err


rotateLeft :: Double -> Model -> Model
rotateLeft dur model =
    let angle' = angle model - realToFrac dur * rotationSpeed
    in model { angle = angle', matrix = makeRotation angle' }

rotateRight :: Double -> Model -> Model
rotateRight dur model =
    let angle' = angle model + realToFrac dur * rotationSpeed
    in model { angle = angle', matrix = makeRotation angle' }

render :: M44 GLfloat -> M44 GLfloat -> Lightning -> Model -> IO ()
render projection view lightning model = do
    GL.glUseProgram (program model)
    GL.glBindVertexArray (vao $ mesh model)

    -- Set uniforms.
    let mvp = projection !*! view !*! matrix model
    GL.setMatrix4 (mvpLoc model) mvp
    GL.setMatrix4 (viewLoc model) view
    GL.setMatrix4 (modelLoc model) (matrix model)
    GL.setVector3 (lightDirLoc model) (lightDir lightning)
    GL.setVector3 (lightColorLoc model) (lightColor lightning)
    GL.glUniform1f (ambientStrengthLoc model) (ambientStrength lightning)
    GL.glUniform1f (specularStrengthLoc model) (specularStrength lightning)
    GL.glUniform1i (specularShineLoc model) (specularShine lightning)

    -- Draw the model.
    GL.drawTrianglesVector (indices $ mesh model)

    GL.glBindVertexArray (VertexArrayObject 0)

loadFileResources :: ModelSpec
                  -> IO (Either String (Mesh, Program, Maybe Texture, Maybe Texture))
loadFileResources spec =
    case spec of
        Spec.ModelSpec _model (Just _texFile) (Just _bumpFile) ->
            return $ Left "Bump mapped specifications not supported yet"

        Spec.ModelSpec model (Just texFile) Nothing         -> do
            mesh' <- meshFromFile model
            prog <- GL.loadShaders
                [ (VertexShader, "shaders/texturedmodel.vert")
                , (FragmentShader, "shaders/texturedmodel.frag")
                ]
            tex <- GL.loadTexture2D RGB8 True texFile
            case (,,) <$> mesh' <*> prog <*> tex of
                Right (mesh'', prog', tex') ->
                    return $ Right (mesh'', prog', Just tex', Nothing)
                Left err -> return $ Left err

        Spec.ModelSpec _model Nothing (Just _bumpFile) ->
            return $ Left "Non valid model specification"

        Spec.ModelSpec _model Nothing Nothing                ->
            return $ Left "Non textured specifications not supported yet"

meshFromFile :: FilePath -> IO (Either String Mesh)
meshFromFile file = do
    vectors <- loadObjFromFile file
    case vectors of
        Right (WithTextureAndNormal vs is) -> Right <$> GL.buildFromVector StaticDraw vs is
        Left err       -> return $ Left err

finalizeModel :: (Mesh, Program, Maybe Texture, Maybe Texture) -> IO Model
finalizeModel (mesh', program', texture', bumpMap') = do
    mvpLoc' <- GL.glGetUniformLocation program' "mvp"
    viewLoc' <- GL.glGetUniformLocation program' "view"
    modelLoc' <- GL.glGetUniformLocation program' "model"
    lightDirLoc' <- GL.glGetUniformLocation program' "lightDir"
    lightColorLoc' <- GL.glGetUniformLocation program' "lightColor"
    ambientStrengthLoc' <- GL.glGetUniformLocation program' "ambientStrength"
    specularStrengthLoc' <- GL.glGetUniformLocation program' "specularStrength"
    specularShineLoc' <- GL.glGetUniformLocation program' "specularShine"

    GL.glBindVertexArray (VertexArrayObject 0)

    return Model
        { program = program'
        , mvpLoc = mvpLoc'
        , viewLoc = viewLoc'
        , modelLoc = modelLoc'
        , lightDirLoc = lightDirLoc'
        , lightColorLoc = lightColorLoc'
        , ambientStrengthLoc = ambientStrengthLoc'
        , specularStrengthLoc = specularStrengthLoc'
        , specularShineLoc = specularShineLoc'
        , mesh = mesh'
        , texture = texture'
        , bumpMap = bumpMap'
        , angle = 0
        , matrix = makeRotation 0
        }

makeRotation :: GLfloat -> M44 GLfloat
makeRotation angle' =
    mkTransformation (axisAngle (V3 0 1 0) angle') (V3 0 0 0)

-- | Half circle per second.
rotationSpeed :: GLfloat
rotationSpeed = pi
