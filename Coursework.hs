import           Control.Monad
import           Control.Applicative
import qualified Data.Text.IO as Text
import           Graphics.Rendering.OpenGL.GL
import           Graphics.UI.GLUT
import           System.Environment
import           System.Exit

import           Parsing


initGL :: IO ()
initGL = do
  clearColor $= Color4 0 0 0 0
  putStrLn "init"


display :: DisplayCallback
display = do
  clear [ColorBuffer, DepthBuffer]
  putStrLn "display"


reshape :: ReshapeCallback
reshape size = do
  putStrLn "reshape"

  viewport $= (Position 0 0, size)


keyboard :: KeyboardCallback
keyboard key (Position _x _y) = case key of
  '\ESC' -> exitSuccess
  _      -> return ()


initGraphics :: String -> [String] -> IO ()
initGraphics progName args = do
  initialDisplayCapabilities $= [ With DisplaySingle,
                                  With DisplayRGB,
                                  With DisplayDepth ]

  initialWindowSize $= Size 256 256
  initialWindowPosition $= Position 0 0

  _ <- initialize progName args

  _ <- createWindow progName

  initGL

  displayCallback $= display
  reshapeCallback $= Just reshape
  keyboardCallback $= Just keyboard

  mainLoop


main :: IO ()
main = do
  -- Text.getContents >>= (print . parse vtkParser)

  args <- getArgs
  progName <- getProgName

  initGraphics progName args
