{-# LANGUAGE NamedFieldPuns #-}

import           Control.Applicative
import           Control.Monad.Primitive
import           Data.Attoparsec.Text as A
import           Data.IORef
import           Data.Monoid
import qualified Data.Text.IO as Text
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import           Data.Vector.Unboxed ((!))
import qualified Data.Vector.Unboxed.Mutable as VUM
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
  position (Light 0) $= Vertex4 1 1 1 0
  -- ambient  (Light 0) $= Color4 0 1 0 0
  -- diffuse  (Light 0) $= Color4 0 1 0 0
  -- specular (Light 0) $= Color4 0 1 0 0

  -- Set material parameters
  -- materialSpecular  FrontAndBack $= Color4 1 0 0 0
  -- materialShininess FrontAndBack $= 0.6

  -- Enable Z-buffering
  -- equivalent of glEnable(GL_DEPTH_TEST)
  -- depthMask $= Enabled
  -- depthFunc $= Just Lequal


changeVector :: (VUM.Unbox a, PrimMonad m) => VUM.MVector (PrimState m) a -> Int -> (a -> a) -> m ()
changeVector v i f = VUM.read v i >>= (VUM.write v i . f)


display :: VTK -> VertexNormals -> DisplayCallback
display (VTK { polygons, vertices }) vertexNormals = do
  clear [ColorBuffer, DepthBuffer]
  putStrLn "display"

  -- TODO don't draw the thing from a different angle, just rotate the world after drawing once?
  V.forM_ polygons $ \(Input.Polygon p) -> renderPrimitive GL.Polygon $ do
    U.forM_ p $ \iVertex -> do
      let Vertex v1 v2 v3 =          vertices      ! iVertex
      let Vertex n1 n2 n3 = vnormal (vertexNormals ! iVertex)
      normal (Normal3 (realToFrac n1) (realToFrac n2) (realToFrac n3) :: Normal3 GLdouble)
      vertex (Vertex3 (realToFrac v1) (realToFrac v2) (realToFrac v3) :: Vertex3 GLdouble)

  flush
  -- swapBuffers
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


reshape :: VTK -> Vertex3 GLdouble -> IORef Input.Vertex -> ReshapeCallback
reshape vtk core eyeRef size = do
  putStrLn "reshape"

  viewport $= (Position 0 0, size)

  transform core eyeRef


keyboard :: KeyboardCallback
keyboard key (Position _x _y) = case key of
  '\ESC' -> exitSuccess
  c      -> putStrLn $ "key code " ++ show c


specialKeyboard :: VTK -> Vertex3 GLdouble -> IORef Input.Vertex -> SpecialCallback
specialKeyboard vtk core eyeRef key (Position _x _y) = do
  case key of
    KeyDown -> modifyIORef eyeRef (+++ Vertex 2 0 0)
    KeyUp   -> modifyIORef eyeRef (+++ Vertex (-2) 0 0)
    -- KeyLeft -> modifyIORef eyeRef (+++ Vertex 0 2 0)
    -- KeyRight -> modifyIORef eyeRef (+++ Vertex 0 (-2) 0)
    KeyLeft -> modifyIORef eyeRef (+++ Vertex 0 0 2)
    KeyRight -> modifyIORef eyeRef (+++ Vertex 0 0 (-2))

    c       -> putStrLn $ "special key code " ++ show c

  transform core eyeRef
  postRedisplay Nothing


transform :: Vertex3 GLdouble -> IORef Input.Vertex -> IO ()
transform core eyeRef = do

  eye@(Vertex eyex eyey eyez) <- readIORef eyeRef

  putStrLn $ "KeyLeft " ++ show eye

  let f = realToFrac

  matrixMode $= Projection
  loadIdentity
  perspective 2 2 1 200

  matrixMode $= Modelview 0
  loadIdentity
  lookAt (Vertex3 (f eyex) (f eyey) (f eyez)) core (Vector3 0 1 0)



type VertexNormals = U.Vector Input.Vertex

calculateVertexNormals :: VTK -> VertexNormals
calculateVertexNormals VTK { vertices, polygons } = U.create $ do
  normalSums <- VUM.replicate (U.length vertices) mempty
  V.forM_ polygons $ \(Input.Polygon p) -> do
    let a = vertices ! (p ! 0)
        b = vertices ! (p ! 1)
        c = vertices ! (p ! 2)
        n = vnormal $ cross (b +-+ a) (c +-+ a)
    U.forM_ p $ \iVertex ->
      changeVector normalSums iVertex (+++ n) -- TODO deepseq mappend thunk
  return normalSums


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

  -- Average center vertex
  let VTK { vertices } = vtk
      num  = fromIntegral (U.length vertices)
      Vertex a b c = (/ num) `vmap` (U.foldl' (+++) mempty vertices)
      f = realToFrac
      core = Vertex3 (f a) (f b) (f c)
  eyeRef <- newIORef (Vertex 10 0 0)

  -- Initialize callback functions
  displayCallback $= display vtk vertexNormals
  reshapeCallback $= Just (reshape vtk core eyeRef)
  keyboardCallback $= Just keyboard
  specialCallback $= Just (specialKeyboard vtk core eyeRef)

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
