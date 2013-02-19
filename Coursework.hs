{-# LANGUAGE NamedFieldPuns #-}

import           Control.Applicative
import           Control.Monad.Primitive
import           Data.Attoparsec.Text as A
import           Data.Monoid
import qualified Data.Text.IO as Text
import qualified Data.Vector as V
import           Data.Vector ((!), create)
import qualified Data.Vector.Mutable as VM
import           Graphics.Rendering.OpenGL.GL
import           Graphics.UI.GLUT as GL
import           System.Environment
import           System.Exit

import           Parsing as Input


initGL :: IO ()
initGL = do
  clearColor $= Color4 0 0 0 0
  putStrLn "init"

  shadeModel $= Smooth

  -- Enable lighting
  lighting        $= Enabled
  light (Light 0) $= Enabled
  position (Light 0) $= Vertex4 0 1 0 0
  -- ambient  (Light 0) $= Color4 0 1 0 0
  -- diffuse  (Light 0) $= Color4 0 1 0 0
  -- specular (Light 0) $= Color4 0 1 0 0

  -- Set material parameters
  materialSpecular  FrontAndBack $= Color4 1 0 0 0
  materialShininess FrontAndBack $= 1

  -- Enable Z-buffering
  -- equivalent of glEnable(GL_DEPTH_TEST)
  -- depthMask $= Enabled
  -- depthFunc $= Just Lequal


changeVector :: PrimMonad m => VM.MVector (PrimState m) a -> Int -> (a -> a) -> m ()
changeVector v i f = VM.read v i >>= (VM.write v i . f)


display :: VTK -> VertexNormals -> DisplayCallback
display (VTK { polygons, vertices }) vertexNormals = do
  clear [ColorBuffer, DepthBuffer]
  putStrLn "display"

  V.forM_ polygons $ \(Input.Polygon p) -> renderPrimitive GL.Polygon $ do
    V.forM_ p $ \iVertex -> do
      let Vertex v1 v2 v3 = vertices      ! iVertex
      let Vertex n1 n2 n3 = vertexNormals ! iVertex
      normal (Normal3 (realToFrac n1) (realToFrac n2) (realToFrac n3) :: Normal3 GLdouble)
      vertex (Vertex3 (realToFrac v1) (realToFrac v2) (realToFrac v3) :: Vertex3 GLdouble)

  flush
  putStrLn "drawing done"

  {-
  -- for all polygons
     renderPrimitive Polygon $ do
       -- for all vertices of polygon
          -- Define texture coordinates of vertex
          texCoord (TexCoord2 0 0 :: TexCoord2 GLfloat)
          -- Define normal of vertex
          normal (Normal3 0 0 0 :: Normal3 GLfloat)
          -- Define coordinates of vertex
          vertex (Vertex3 0 0 0 :: Vertex3 GLfloat)
  flush
  -- or, if double buffering is used,
  -- swapBuffers
  -}


reshape :: ReshapeCallback
reshape size = do
  putStrLn "reshape"

  viewport $= (Position 0 0, size)

  {-
  matrixMode $= Projection
  loadIdentity
  perspective fovy aspect near far
  matrixMode $= Modelview 0 -- vertex unit 0 correct here?
  loadIdentity
  lookAt (Vertex3 eyex eyey eyez) (Vertex3 centerx centery centerz) (Vector3 upx upy upz)
  -}


keyboard :: KeyboardCallback
keyboard key (Position _x _y) = case key of
  '\ESC' -> exitSuccess
  c      -> putStrLn $ "key code " ++ show c


specialKeyboard :: VTK -> VertexNormals -> SpecialCallback
specialKeyboard vtk vertexNormals key (Position _x _y) = case key of
  KeyLeft -> do
    matrixMode $= Projection
    -- loadIdentity
    rotate (- 5.0) (Vector3 0 1 0 :: Vector3 GLfloat)
    display vtk vertexNormals
  c       -> putStrLn $ "special key code " ++ show c


type VertexNormals = V.Vector Input.Vertex

calculateVertexNormals :: VTK -> VertexNormals
calculateVertexNormals VTK { vertices, polygons } = create $ do
  normalSums <- VM.replicate (V.length vertices) mempty
  V.forM_ polygons $ \(Input.Polygon p) -> do
    let a = vertices ! (p ! 0)
        b = vertices ! (p ! 1)
        c = vertices ! (p ! 2)
        n = vnormal $ cross (b +-+ a) (c +-+ a)
    V.forM_ p $ \iVertex ->
      changeVector normalSums iVertex (+++ n) -- TODO deepseq mappend thunk
  return normalSums

  -- TODO normalize vertexNormals?


initGraphics :: String -> [String] -> VTK -> IO ()
initGraphics progName args vtk = do
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

  let vertexNormals = calculateVertexNormals vtk

  -- Initialize callback functions
  displayCallback $= display vtk vertexNormals
  reshapeCallback $= Just reshape
  keyboardCallback $= Just keyboard
  specialCallback $= Just (specialKeyboard vtk vertexNormals)

  -- Start renderingdisplay
  mainLoop


main :: IO ()
main = do
  args     <- getArgs
  progName <- getProgName

  parsedVtk <- parse vtkParser <$> Text.getContents

  case parsedVtk of
    Done _ vtk -> initGraphics progName args vtk
    _          -> error "could not parse vtk"
