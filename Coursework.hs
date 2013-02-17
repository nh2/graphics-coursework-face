{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad
import           Control.Applicative
import           Data.Attoparsec.Text as A
import           Data.Text
import qualified Data.Text.IO as Text
import           Data.Vector


data Vertex = Vertex Double Double Double deriving (Eq, Show)
data Polygon = Polygon (Vector Int) deriving (Eq, Show)
data TextureCoordinate = TextureCoordinate Double Double deriving (Eq, Show)


data VTK = VTK
  { description :: Text
  , vertices :: Vector Vertex
  , polygons :: Vector Polygon
  , textures :: Vector TextureCoordinate
  } deriving (Eq, Show)


vertexParser :: Parser Vertex
vertexParser = Vertex <$> d <*> d <*> d
  where
    d = signed double <* skipSpace

polygonParser :: Parser Polygon
polygonParser = do
  size <- decimal
  Polygon . fromList <$> A.count size (skipSpace *> decimal)

textureCoordParser :: Parser TextureCoordinate
textureCoordParser = TextureCoordinate <$> (skipSpace *> double) <*> (skipSpace *> double)

vtkParser :: Parser VTK
vtkParser = do
  _ <- "# vtk DataFile Version 3.0" *> endl
  desc <- endl
  nPoints <- ("ASCII" *> endl *> "DATASET POLYDATA" *> endl *> "POINTS ") *> decimal <*. " float" <* endl
  verts <- A.count nPoints (vertexParser <* skipSpace)
  nPolys <- "POLYGONS " .*> decimal <* endl
  polys <- A.count nPolys (polygonParser <* endl)
  nTextures <- "POINT_DATA " *> decimal <* endl
  _ <- endl
  when (nTextures /= nPoints) $ error "number of textures and points mismatch"
  texts <- A.count nTextures textureCoordParser
  return $ VTK desc (fromList verts) (fromList polys) (fromList texts)
  where
    endl = takeTill isEndOfLine <* endOfLine


main :: IO ()
main = do
  Text.getContents >>= (print . parse vtkParser)
