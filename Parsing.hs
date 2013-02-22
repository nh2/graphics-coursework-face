{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies, MultiParamTypeClasses, TemplateHaskell #-}

module Parsing where

import           Control.Monad
import           Control.Applicative
import           Data.Attoparsec.Text as A
import           Data.Monoid
import           Data.Text
import           Data.Vector as V

import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Generic
import qualified Data.Vector.Generic.Mutable

import Data.Vector.Unboxed.Deriving


-- | A point in 3D space.
data Vertex = Vertex {-# UNPACK #-} !Double
                     {-# UNPACK #-} !Double
                     {-# UNPACK #-} !Double deriving (Eq, Show)

-- | A polygon in 3D space. Must be planar.
-- Indices into a list of vertices, starting with 0.
data Polygon = Polygon (U.Vector Int) deriving (Eq, Show)

-- | A floating-point coordinate in a 2D texture.
data TextureCoordinate = TextureCoordinate Double Double deriving (Eq, Show)


-- Make vertices fit into packed vectors.
derivingUnbox "Vertex"
    [t| Vertex -> (Double, Double, Double) |]
    [| \ (Vertex a b c) -> (a, b, c) |]
    [| \ (a, b, c) -> Vertex a b c |]


-- Vertices have a zero-element (0,0,0) an an additive function (component-wise plus).
instance Monoid Vertex where
  mempty = Vertex 0 0 0
  mappend = (+++)


-- | Applies a function to all three components of the vertex.
vmap :: (Double -> Double) -> Vertex -> Vertex
vmap f (Vertex a b c) = Vertex (f a) (f b) (f c)

-- | Euclidean norm / "length" of the vertex.
vabs :: Vertex -> Double
vabs (Vertex a b c) = sqrt (a*a + b*b + c*c)

-- | Normalizes the vector to length 1.
vnormal :: Vertex -> Vertex
vnormal v = (/ vabs v) `vmap` v

-- | Addition, substraction, cross product of vertices.
(+++), (+-+), cross :: Vertex -> Vertex -> Vertex
Vertex a b c   +++   Vertex x y z = Vertex (a+x) (b+y) (c+z)
Vertex a b c   +-+   Vertex x y z = Vertex (a-x) (b-y) (c-z)
Vertex a b c `cross` Vertex x y z = Vertex (b * z - c * y)
                                           (c * x - a * z)
                                           (a * y - b * x)


-- | The data contained in a VTK file.
data VTK = VTK
  { description :: Text
  , vertices :: U.Vector Vertex
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
  verts <- U.replicateM nPoints (vertexParser <* skipSpace)

  -- Parse polygons
  nPolys <- "POLYGONS " .*> decimal <* endl
  polys <- V.replicateM nPolys (polygonParser <* endl)

  -- Parse texture coordinates
  nTextures <- "POINT_DATA " *> decimal <* endl

  _ <- endl -- TEXTURE COORDINATES line
  when (nTextures /= nPoints) $ error "number of textures and points mismatch"
  texts <- V.replicateM nTextures textureCoordParser

  -- Done
  return $ VTK desc verts polys texts
  where
    -- Eat input until the end of line. Consumes but discards the end of line.
    endl = takeTill isEndOfLine <* endOfLine
    sd = signed double <* skipSpace -- A signed double. Discards trailing whitespace.

    -- How to parse single Vertices, Polygons, texture coordinates.
    vertexParser = Vertex <$> sd <*> sd <*> sd
    polygonParser = do
      size <- decimal
      Polygon <$> U.replicateM size (skipSpace *> decimal)
    textureCoordParser = TextureCoordinate <$> (skipSpace *> double) <*> (skipSpace *> double)
