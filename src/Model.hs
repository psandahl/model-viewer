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
import           Graphics.OBJ
import           Linear        (M44, V3 (..), axisAngle, mkTransformation,
                                (!*!))

import           Lightning     (Lightning (..))
import           ModelSpec     (ModelSpec)
import qualified ModelSpec     as Spec

data Model = Model
    { program            :: !Program
    , mvpLoc             :: !Location
    , viewLoc            :: !Location
    , modelLoc           :: !Location
    , lightDirLoc        :: !Location
    , lightColorLoc      :: !Location
    , ambientStrengthLoc :: !Location
    , diffuseStrengthLoc :: !Location
    , mesh               :: !Mesh
    , texture            :: !(Maybe Texture)
    , bumpMap            :: !(Maybe Texture)
    , angle              :: !GLfloat
    , matrix             :: !(M44 GLfloat)
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
                    viewLoc' <- GL.glGetUniformLocation program' "view"
                    modelLoc' <- GL.glGetUniformLocation program' "model"
                    lightDirLoc' <- GL.glGetUniformLocation program' "lightDir"
                    lightColorLoc' <- GL.glGetUniformLocation program' "lightColor"
                    ambientStrengthLoc' <- GL.glGetUniformLocation program' "ambientStrength"
                    diffuseStrengthLoc' <- GL.glGetUniformLocation program' "diffuseStrength"

                    GL.glBindVertexArray (VertexArrayObject 0)

                    return $ Right Model
                        { program = program'
                        , mvpLoc = mvpLoc'
                        , viewLoc = viewLoc'
                        , modelLoc = modelLoc'
                        , lightDirLoc = lightDirLoc'
                        , lightColorLoc = lightColorLoc'
                        , ambientStrengthLoc = ambientStrengthLoc'
                        , diffuseStrengthLoc = diffuseStrengthLoc'
                        , mesh = mesh'
                        , texture = Just texture'
                        , bumpMap = Nothing
                        , angle = 0
                        , matrix = makeRotation 0
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
    GL.glUniform1f (diffuseStrengthLoc model) (diffuseStrength lightning)

    -- Draw the model.
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

makeRotation :: GLfloat -> M44 GLfloat
makeRotation angle' =
    mkTransformation (axisAngle (V3 0 1 0) angle') (V3 0 0 0)

-- | Half circle per second.
rotationSpeed :: GLfloat
rotationSpeed = pi

{-loadBumpMapFromFile :: ModelSpec -> IO (Either String Texture)
loadBumpMapFromFile spec =
    case Spec.bumpMap spec of
        Just f  -> GL.loadTexture2D RGB8 False f
        Nothing -> return $ Left "No file specified"
-}
