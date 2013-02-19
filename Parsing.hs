{-# LANGUAGE OverloadedStrings #-}

module Parsing where

import           Control.Monad
import           Control.Applicative
import           Data.Attoparsec.Text as A
import           Data.Monoid
import           Data.Text
import           Data.Vector


-- TODO ! and unbox
data Vertex = Vertex Double Double Double deriving (Eq, Show)
data Polygon = Polygon (Vector Int) deriving (Eq, Show)
data TextureCoordinate = TextureCoordinate Double Double deriving (Eq, Show)


instance Monoid Vertex where
  mempty = Vertex 0 0 0
  Vertex x y z `mappend` Vertex x' y' z' = Vertex (x+x') (y+y') (z+z')


vmap :: (Double -> Double) -> Vertex -> Vertex
vmap f (Vertex a b c) = Vertex (f a) (f b) (f c)

vabs :: Vertex -> Double
vabs (Vertex a b c) = sqrt (a*a + b*b + c*c)

vnormal :: Vertex -> Vertex
vnormal v = (/ vabs v) `vmap` v

(+++), (+-+), cross :: Vertex -> Vertex -> Vertex
Vertex a b c   +++   Vertex x y z = Vertex (a+x) (b+y) (c+z)
Vertex a b c   +-+   Vertex x y z = Vertex (a-x) (b-y) (c-z)
Vertex a b c `cross` Vertex x y z = Vertex (b * z - c * y)
                                           (c * x - a * z)
                                           (a * y - b * x)


data VTK = VTK
  { description :: Text
  , vertices :: Vector Vertex
  , polygons :: Vector Polygon
  , textures :: Vector TextureCoordinate
  } deriving (Eq, Show)


-- | Parses the contents of a .vtk file into a VTK data structure.
vtkParser :: Parser VTK
vtkParser = do
  -- Parse header
  _ <- "# vtk DataFile Version 3.0" *> endl
  desc <- endl <* ("ASCII" *> endl *> "DATASET POLYDATA" *> endl)
  -- Parse points
  nPoints <- "POINTS " .*> decimal <*. " float" <* endl
  verts <- A.count nPoints (vertexParser <* skipSpace)
  -- Parse polygons
  nPolys <- "POLYGONS " .*> decimal <* endl
  polys <- A.count nPolys (polygonParser <* endl)
  -- Parse texture coordinates
  nTextures <- "POINT_DATA " *> decimal <* endl
  _ <- endl -- TEXTURE COORDINATES line
  when (nTextures /= nPoints) $ error "number of textures and points mismatch"
  texts <- A.count nTextures textureCoordParser
  -- Done
  return $ VTK desc (fromList verts) (fromList polys) (fromList texts)
  where
    endl = takeTill isEndOfLine <* endOfLine
    sd = signed double <* skipSpace

    -- How to parse single Vertices, Polygons, texture coordinates
    vertexParser = Vertex <$> sd <*> sd <*> sd
    polygonParser = do
      size <- decimal
      Polygon . fromList <$> A.count size (skipSpace *> decimal)
    textureCoordParser = TextureCoordinate <$> (skipSpace *> double) <*> (skipSpace *> double)
