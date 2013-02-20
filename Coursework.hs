{-# LANGUAGE NamedFieldPuns #-}

import           Control.Applicative
import           Control.Monad
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


deg :: (Num a, Floating a) => a
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


reshape :: State -> ReshapeCallback
reshape state size = do
  putStrLn "reshape"

  viewport $= (Position 0 0, size)

  transform state


keyboardMouse :: State -> KeyboardMouseCallback
keyboardMouse state@State { horizAngleVar, vertAngleVar, distanceVar } key keyState _mods (Position _x _y) = do
  when (keyState == Down) $ case key of
    SpecialKey KeyLeft  -> horizAngleVar $~! (+    2 *deg ) >> redraw
    SpecialKey KeyRight -> horizAngleVar $~! (+ (- 2 *deg)) >> redraw
    SpecialKey KeyUp    -> vertAngleVar  $~! (+    2 *deg ) >> redraw
    SpecialKey KeyDown  -> vertAngleVar  $~! (+ (- 2 *deg)) >> redraw

    MouseButton WheelUp   -> distanceVar $~! (+ (- 5)) >> redraw
    MouseButton WheelDown -> distanceVar $~! (+    5 ) >> redraw

    Char '\ESC' -> exitSuccess

    k -> putStrLn $ "unhandled key pressed: " ++ show k
  where
    redraw = transform state >> postRedisplay Nothing


transform :: State -> IO ()
transform State { horizAngleVar, vertAngleVar, distanceVar, centerVar } = do

  horizAngle <- get horizAngleVar
  vertAngle  <- get vertAngleVar
  d          <- get distanceVar
  center     <- get centerVar
  let eye = realToFrac <$> Vertex3 (d * sin horizAngle)
                                   (d * sin vertAngle )
                                   (d * cos horizAngle)

  matrixMode $= Projection
  loadIdentity
  perspective 1 2 1 200

  matrixMode $= Modelview 0
  loadIdentity
  lookAt eye center (Vector3 0 1 0)


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
      num              = fromIntegral (U.length vertices)
      Vertex a b c     = (/ num) `vmap` (U.foldl' (+++) mempty vertices)
      f                = realToFrac
      center           = Vertex3 (f a) (f b) (f c)

  -- Initialize global variables
  state <- State <$> newIORef (30 *deg) <*> newIORef 0 <*> newIORef 20 <*> newIORef center

  -- Initialize callback functions
  displayCallback $= display vtk vertexNormals
  reshapeCallback $= Just (reshape state)
  keyboardMouseCallback $= Just (keyboardMouse state)

  -- Start renderingdisplay
  mainLoop


type VertexNormals = U.Vector Input.Vertex

data State = State
  { horizAngleVar :: IORef Double
  , vertAngleVar :: IORef Double
  , distanceVar :: IORef Double
  , centerVar :: IORef (Vertex3 GLdouble)
  }


main :: IO ()
main = do
  args     <- getArgs
  progName <- getProgName

  parsedVtk <- parse vtkParser <$> Text.getContents

  case parsedVtk of
    Done _ vtk -> initGraphics progName args vtk
    _          -> error "could not parse vtk"
