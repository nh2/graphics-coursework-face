import           Control.Monad
import           Control.Applicative
import           Data.Attoparsec.Text as A
import qualified Data.Text.IO as Text

import           Parsing

main :: IO ()
main = do
  Text.getContents >>= (print . parse vtkParser)
