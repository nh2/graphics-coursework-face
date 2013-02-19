import           Control.Monad
import           Graphics.Rendering.OpenGL.GL
import           Graphics.UI.GLUT
import           System.Environment
import           System.Exit


initGL :: IO ()
initGL = do
  clearColor $= Color4 0 0 0 0
  putStrLn "init"

  {-
  shadeModel $= Smooth

  -- Enable lighting
  lighting        $= Enabled
  light (Light 0) $= Enabled
  position (Light 0) $= Vertex4 0 0 0 0
  ambient  (Light 0) $= Color4 0 0 0 0
  diffuse  (Light 0) $= Color4 0 0 0 0
  specular (Light 0) $= Color4 0 0 0 0

  -- Set material parameters
  materialSpecular  FrontAndBack $= Color4 0 0 0 0
  materialShininess FrontAndBack $= 0

  -- Enable Z-buffering
  -- equivalent of glEnable(GL_DEPTH_TEST)
  depthMask $= Enabled
  depthFunc $= Just Lequal
  -}


display :: DisplayCallback
display = do
  clear [ColorBuffer, DepthBuffer]
  putStrLn "display"

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
  _      -> return ()


initGraphics :: String -> [String] -> IO ()
initGraphics progName args = do
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

  -- Initialize callback functions
  displayCallback $= display
  reshapeCallback $= Just reshape
  keyboardCallback $= Just keyboard

  -- Start rendering
  mainLoop


main :: IO ()
main = do
  args <- getArgs
  progName <- getProgName

  initGraphics progName args
