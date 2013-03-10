{-# LANGUAGE NamedFieldPuns #-}

-- Yeah nice. If we give this module a name,
-- rendering is 10x slower due to non-inlining of
-- automatically exported functions like `changeVector`.
-- Empty export list and INLINE pragmas help.
-- module Main where

import           Control.Applicative
import           Control.Monad
import           Data.Attoparsec.Text as A
import qualified Data.ByteString as BS
import           Data.IORef
import           Data.Monoid
import qualified Data.Text.IO as Text
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import           Data.Vector.Storable (convert)
import qualified Data.Vector.Unboxed as U
import           Data.Vector.Unboxed ((!))
import qualified Data.Vector.Unboxed.Mutable as VUM
import           Graphics.Rendering.OpenGL.GL
import           Graphics.Netpbm
import           Graphics.UI.GLUT as GL
import qualified Options.Applicative as Args
import           Options.Applicative (argument, str, metavar, info, fullDesc)
import           System.Environment
import           System.Exit

import           Parsing as Input


{-# INLINE deg #-}
deg :: (Floating a) => a
deg = 2 * pi / 180


initGL :: IO ()
initGL = do
  clearColor $= Color4 0 0 0 0
  putStrLn "init"

  shadeModel $= Smooth

  -- Enable lighting
  lighting        $= Enabled
  light (Light 0) $= Enabled
  position (Light 0) $= Vertex4 1 1 1 0.1
  -- ambient  (Light 0) $= Color4 1 1 1 0
  -- diffuse  (Light 0) $= Color4 1 1 1 0
  -- specular (Light 0) $= Color4 1 1 1 0

  -- Set material parameters
  -- materialSpecular  FrontAndBack $= Color4 1 0 0 0
  -- materialShininess FrontAndBack $= 0.6

  -- Enable Z-buffering
  -- equivalent of glEnable(GL_DEPTH_TEST)
  depthMask $= Enabled
  depthFunc $= Just Lequal

  -- Enable texturing
  texture Texture2D $= Enabled


-- | The "global state" of the scene.
data State = State
  { horizAngleVar       :: IORef Double -- ^ Left/right rotation around the object
  , vertAngleVar        :: IORef Double -- ^ Up/down rotation around the object
  , distanceVar         :: IORef Double -- ^ View distance to the object
  , centerVar           :: IORef (Vertex3 GLdouble) -- ^ Center ob the shown object. Targeted by camera.
  , lastMouseDragPosVar :: IORef (Maybe Position)   -- ^ Last absolute mouse position in the window. Only set in a drag.
  }


-- | The normal vector on each vertex.
type VertexNormals = U.Vector Input.Vertex


-- | Called to render a full scene.
display :: State -> VTK -> VertexNormals -> DisplayCallback
display state (VTK { polygons, vertices, textures }) vertexNormals = do
  clear [ColorBuffer, DepthBuffer]
  putStrLn "display"

  -- Apply rotations of objects and camera
  transform state

  V.forM_ polygons $ \(Input.Polygon p) -> renderPrimitive GL.Polygon $ do
    U.forM_ p $ \iVertex -> do
      let Vertex v1 v2 v3 = vertices      ! iVertex
      let Vertex n1 n2 n3 = vertexNormals ! iVertex
      normal (Normal3 (realToFrac n1) (realToFrac n2) (realToFrac n3) :: Normal3 GLdouble)
      vertex (Vertex3 (realToFrac v1) (realToFrac v2) (realToFrac v3) :: Vertex3 GLdouble)

      let TextureCoordinate x y = textures V.! iVertex
      texCoord (TexCoord2 (realToFrac x) (realToFrac y) :: TexCoord2 GLfloat)
  flush
  -- swapBuffers -- for double-buffering
  putStrLn "drawing done"


-- | Applies view transformation (scene rotation + camera).
transform :: State -> IO ()
transform State { horizAngleVar, vertAngleVar, distanceVar, centerVar } = do

  horizAngle <- get horizAngleVar
  vertAngle  <- get vertAngleVar
  d          <- get distanceVar
  center     <- get centerVar
  let eye = realToFrac <$> Vertex3 (d * sin horizAngle)
                                   (d * sin vertAngle )
                                   (d * cos horizAngle)

  Size w h <- get windowSize

  matrixMode $= Projection
  loadIdentity
  perspective (30 *deg) (fromIntegral w / fromIntegral h) 1 200

  matrixMode $= Modelview 0
  loadIdentity
  lookAt eye center (Vector3 0 1 0)


-- | Requests the scene to be redrawn with the given state.
redraw :: IO ()
redraw = postRedisplay Nothing


-- | Called on window resize. Redraws automatically.
reshape :: ReshapeCallback
reshape size = do
  putStrLn "reshape"

  -- Apply new window size
  viewport $= (Position 0 0, size)


-- | Called when keyboard or mouse buttons are pressed.
keyboardMouse :: State -> KeyboardMouseCallback
keyboardMouse state@State { horizAngleVar, vertAngleVar, distanceVar, lastMouseDragPosVar } = on
  where
    on (SpecialKey KeyLeft)    Down _ _ = horizAngleVar $~! (+    2 *deg ) >> redraw
    on (SpecialKey KeyRight)   Down _ _ = horizAngleVar $~! (+ (- 2 *deg)) >> redraw
    on (SpecialKey KeyUp)      Down _ _ = vertAngleVar  $~! (+    2 *deg ) >> redraw
    on (SpecialKey KeyDown)    Down _ _ = vertAngleVar  $~! (+ (- 2 *deg)) >> redraw

    on (MouseButton WheelUp)   Down _ _ = distanceVar $~! (+ (- 5)) >> redraw
    on (MouseButton WheelDown) Down _ _ = distanceVar $~! (+    5 ) >> redraw
    on (MouseButton _)         Up   _ _ = lastMouseDragPosVar $= Nothing -- Reset drag on mouse up

    on (Char '\ESC')           Down _ _ = exitSuccess

    on k                       _    _ _ = putStrLn $ "unhandled key pressed: " ++ show k


-- | Called when the mouse is moved.
mouseMotion :: State -> MotionCallback
mouseMotion State { lastMouseDragPosVar, horizAngleVar, vertAngleVar } pos@(Position x y) = do
  lastDragPos <- get lastMouseDragPosVar

  -- Always calculate drag from *drag start position*
  case lastDragPos of
    Nothing               -> lastMouseDragPosVar $= Just pos
    Just (Position lx ly) -> do

      horizAngleVar $~! (+ fromIntegral (-(x - lx)) / 2 *deg)
      vertAngleVar  $~! (+ fromIntegral (  y - ly ) / 2 *deg)

      lastMouseDragPosVar $= Just pos

      redraw


-- | Calculates the average normal vector on each vector (normalized to length 1).
calculateVertexNormals :: VTK -> VertexNormals
calculateVertexNormals VTK { vertices, polygons } = U.create $ do
  let nVertices = U.length vertices
  normalSums <- VUM.replicate nVertices mempty
  V.forM_ polygons $ \(Input.Polygon p) -> do
    -- Take the first 3 points of the polygon, (a, b, c), to calculat the normal on the polygon.
    let a = vertices ! (p ! 0)
        b = vertices ! (p ! 1)
        c = vertices ! (p ! 2)
        n = vnormal $ cross (b +-+ a) (c +-+ a)
    -- Add the polygon normal to the sum of normals for each vertex in the polygon.
    U.forM_ p $ \iVertex ->
      changeVector normalSums (+++ n) iVertex -- TODO deepseq mappend thunk?
  -- Normalize each vertex normal to length 1.
  mapChangeVector normalSums vnormal
  return normalSums
  where
    -- Change a mutable vector at the given index / at all indices.
    changeVector v f i = VUM.read v i >>= (VUM.write v i . f)
    mapChangeVector v f = forM_ [0..VUM.length v - 1] $ changeVector v f



initGraphics :: String -> [String] -> VTK -> PPM -> IO ()
initGraphics progName args vtk ppm = do
  initialDisplayCapabilities $= [ With DisplaySingle, -- or double buffering: DisplayDouble
                                  With DisplayRGB,
                                  With DisplayDepth ]

  initialWindowSize $= Size 256 256
  initialWindowPosition $= Position 0 0

  -- Initialize graphics window
  _ <- initialize progName args
  _ <- createWindow progName

  -- Initialize OpenGL
  initGL

  -- NOTE: Not using let here because then strange inlining recomputes this on *every* frame. GHC bug?
  vertexNormals <- return $ calculateVertexNormals vtk

  -- Average center vertex
  let VTK { vertices } = vtk
      num              = fromIntegral (U.length vertices)
      Vertex a b c     = (/ num) `vmap` (U.foldl' (+++) mempty vertices)
      f                = realToFrac
      center           = Vertex3 (f a) (f b) (f c)

  -- Initialize global state
  state <- State <$> newIORef (30 *deg) -- on-ground rotation angle
                 <*> newIORef 0         -- into-the-air angle
                 <*> newIORef 20        -- the distance to the center
                 <*> newIORef center    -- the point around which we rotate ("center")
                 <*> newIORef Nothing   -- last mouse position

  -- Initialize input and rendering callback functions
  displayCallback       $= display state vtk vertexNormals
  reshapeCallback       $= Just reshape
  keyboardMouseCallback $= Just (keyboardMouse state)
  motionCallback        $= Just (mouseMotion   state)

  {- Bind texture -}

  let PPM { ppmWidth, ppmHeight, ppmData } = ppm
      -- Convert pixel vector to a Vector.Storable vector from which we can get a pointer for OpeGL
      storableVec = case ppmData of
                      PpmPixelDataRGB8 pixelVector -> convert pixelVector
                      _                            -> error "can only load 8-bits-per-color textures"
      textureSize = TextureSize2D (fromIntegral ppmWidth) (fromIntegral ppmHeight)

  -- Apply texture settings
  textureBinding Texture2D $= Nothing
  textureWrapMode Texture2D S $= (Repeated, Repeat)
  textureWrapMode Texture2D Q $= (Repeated, Repeat)
  textureFunction $= Modulate
  textureFilter Texture2D $= ((Linear', Nothing), Linear') -- simple GL_LINEAR

  -- Connect texture coordinates with pixel data
  VS.unsafeWith storableVec $ \pixelPtr ->
    texImage2D Nothing NoProxy 0 RGB' textureSize 0 (PixelData RGB UnsignedByte pixelPtr)

  -- Start rendering
  mainLoop


-- * Command line argument parsing

data Args = Args {
  vtkPath     :: String
, texturePath :: String
}

argsParser :: Args.Parser Args
argsParser = Args <$> argument str (metavar "VTK_FILE")
                  <*> argument str (metavar "TEXTURE_PPM_FILE")


-- | Entry point of the program.
main :: IO ()
main = do
  progName <- getProgName

  -- Parse arguments
  Args { vtkPath, texturePath } <- Args.execParser (info (helper <*> argsParser) fullDesc)

  -- Load points, polygon, texture coordinates from VTK file
  parsedVtk <- parse vtkParser <$> Text.readFile vtkPath

  vtk <- case maybeResult parsedVtk of
    Nothing  -> error "could not parse vtk"
    Just vtk -> return vtk

  -- Load texture from PPM file
  parsedPpm <- parsePPM <$> BS.readFile texturePath

  ppm <- case parsedPpm of
    Left _           -> error "could not parse ppm"
    Right (ppm:_, _) -> return ppm

  initGraphics progName [] vtk ppm
