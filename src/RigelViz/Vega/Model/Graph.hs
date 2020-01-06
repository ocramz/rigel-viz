{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# language LambdaCase #-}
{-# language TemplateHaskell #-}
{-# language DeriveGeneric #-}
{-# language DeriveFunctor #-}
{-# language GeneralizedNewtypeDeriving, DeriveTraversable #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module RigelViz.Vega.Model.Graph where

import Data.Foldable (Foldable(..))
import qualified GHC.Generics as G (Generic(..))

-- aeson
import qualified Data.Aeson as A (ToJSON(..))
-- algebraic-graphs
import Algebra.Graph (Graph(..))
import Algebra.Graph.AdjacencyMap (AdjacencyMap, adjacencyMap, empty, vertex, edge, connect, overlay, hasVertex, hasEdge, preSet, postSet)
-- colour
import qualified Data.Colour as C
import qualified Data.Colour.SRGB as C (RGB(..), toSRGB, sRGB24show)
-- containers
import qualified Data.Map as M (Map, fromList, insert, empty, lookup)
import qualified Data.Set as S (Set)
-- generics-sop
import Generics.SOP.GGP (GDatatypeInfo)
-- microlens
import Lens.Micro (Lens, Lens', Traversal, Traversal', (&))
import Lens.Micro (ASetter, (.~), (?~)) -- setters
import Lens.Micro ((%~)) -- modifiers
import Lens.Micro (Getting, (^.)) -- getters
import Lens.Micro (traverseOf, traversed, _Just) -- traversals
-- microlens-ghc   -- traversals for 'containers' et al.
import Lens.Micro.GHC (at, ix) -- the respective typeclasses are not exported
-- microlens-mtl
import Lens.Micro.Mtl ((.=), (?=), assign) -- setters
import Lens.Micro.Mtl ((%=), zoom) -- modifiers
import Lens.Micro.Mtl (use) -- getters
-- microlens-th
import Lens.Micro.TH
-- mtl
import Control.Monad.State.Class (MonadState(..), gets)
import Control.Monad.State (State, runState, StateT(..), runStateT)

import RigelViz.Vega.Generics (sopFieldNames)



{- INTUITION :

the initial state is specified by the user, e.g. as the following plot declaration

scatter :: Scale x -> Scale x -> Mark x
scatter xs ys =
  circle &
    geom %~ xScale xs &
    geom %~ yScale ys

the compiler then populates all scale, axis, dataset names accordingly, while performing checks on the input value, eg with mapAccumM
-}



-- * Scale

-- -- | All the scales in current scope.
-- --
-- -- An axis requires at least one item to be present in either the X or Y list
-- -- Similarly for the legend (i.e. we need at least one colour scale)
-- data Scales a = Scales {
--   scalesX :: [Scale a]
--   , scalesY :: [Scale a]
--   , scalesCol :: [ColourScale]
--   }

-- ** Domain

data Domain a =
     DomainValues { _domainValues :: [a] }
   | DomainData {
        _domainDataRef :: String
      , _domainDataField :: String 
      } deriving (Eq, Ord, Show, G.Generic, Functor)
makeLenses ''Domain
instance A.ToJSON a => A.ToJSON (Domain a) where

-- ** Range
  
data Range a =
  RangeWidth
  | RangeHeight
  | RangeBounds { _rangeBoundsMin :: a, _rangeBoundsMax :: a}
  deriving (Eq, Ord, Show, G.Generic, Functor)
instance A.ToJSON a => A.ToJSON (Range a)
makeLenses ''Range

-- ** Scale

data ScaleType =
  Linear
  | Ordinal -- ^ discrete, ordered values
  deriving (Eq, Ord, Show, G.Generic)
instance A.ToJSON ScaleType where

data Scale a = Scale {
    _scaleName :: Maybe String -- initially Nothing
  , _scaleType :: ScaleType
  , _scaleDomain :: Domain a
  , _scaleRange :: Range a 
  } deriving (Eq, Ord, Show, G.Generic, Functor)
makeLenses ''Scale
instance A.ToJSON a => A.ToJSON (Scale a) where

-- scale :: ScaleType -> Domain a -> Range a -> Scale a
-- scale = Scale Nothing 

-- | Name of the data reference used by this scale
nameDomainDataRef :: Traversal' (Scale a) String
nameDomainDataRef = scaleDomain . domainDataRef 

-- | Name of the data field used by this scale
nameDomainDataField :: Traversal' (Scale a) String
nameDomainDataField = scaleDomain . domainDataField




-- ** Scale (colour)

data ColourPalette =
  Plasma | Category20 | BlueOrange deriving (Eq, Ord, Show, G.Generic)

newtype Col = Col (C.Colour Double) deriving (Eq, Show)
-- | Shortcut : RGB colours are really a lattice, not a totally ordered field
instance Ord Col where
  (Col c1) <= (Col c2) = colNorm c1 <= colNorm c2

colNorm :: (Floating a, Ord a) => C.Colour a -> a
colNorm c = sqrt $ sum $ zipWith (*) v v where
  v = [r, g, b]
  (C.RGB r g b) = C.toSRGB c


data ColourScaleType =
  ColLinear { _colLinearScaleRange :: ColourPalette }
  | ColOrdinal { _colOrdinalScaleRange :: [Col] }
  deriving (Eq, Ord, Show, G.Generic)
makeLenses ''ColourScaleType

data ColourScale a = ColourScale {
    _colourScaleName :: Maybe String
  , _colourScaleType :: ColourScaleType
  , _colourScaleDomain :: Domain a
  } deriving (Eq, Ord, Show, G.Generic)
makeLenses ''ColourScale

-- instance A.ToJSON (C.Colour a) where





-- ** Features : can be either constant or referring to a scale

-- | A ScalarFeature could be a coordinate or a size annotation, either constant or tied to a data scale
data ScalarFeature x =
    GFConst { _sfConst :: x } -- ^ constant
  | GFFromScale { _sfScale :: Scale x } -- ^ derived from a 'scale'
  deriving (Eq, Ord, Show, G.Generic, Functor)
makeLenses ''ScalarFeature
instance A.ToJSON x => A.ToJSON (ScalarFeature x) where

-- sfConst :: Traversal' (ScalarFeature x) x
  
-- sfScale :: Traversal' (ScalarFeature x) (Scale x)



nameScaleDomainDataRef :: Traversal' (ScalarFeature x) String
nameScaleDomainDataRef = sfScale . nameDomainDataRef

nameScaleDomainDataField :: Traversal' (ScalarFeature x) String
nameScaleDomainDataField = sfScale . nameDomainDataField

-- | A ColFeature is a colour annotation, either constant or tied to a data scale
data ColFeature x =
    CFConst { _cfConst :: Col} -- ^ constant
  | CFFromScale { _cfScale :: ColourScale x } -- ^ derived from a 'scale'
  deriving (Eq, Ord, Show, G.Generic)
makeLenses ''ColFeature

-- instance A.ToJSON ColFeature where  -- FIXME




-- * Geometry features

data GeomFeatureTy = X | Y | X2 | Y2 | Width | Height deriving (Eq, Show, Ord)
newtype GeomFeatures x = GeomFeatures {
  _gfMap :: M.Map GeomFeatureTy (ScalarFeature x) } deriving (Eq, Ord, Show, Functor)
-- newtype GeomFeatures a = GeomFeatures {
--   _gfMap :: M.Map GeomFeatureTy a } deriving (Eq, Show, Functor, Foldable, Traversable)
makeLenses ''GeomFeatures

geomFeatures :: GeomFeatures a
geomFeatures = GeomFeatures M.empty

gfX, gfY, gfX2, gfY2, gfWidth, gfHeight :: Traversal' (GeomFeatures x) (ScalarFeature x)
gfX = gfMap . at X . _Just
gfY = gfMap . at Y . _Just
gfX2 = gfMap . at X2 . _Just
gfY2 = gfMap . at Y2 . _Just
gfWidth = gfMap . at Width . _Just
gfHeight = gfMap . at Height . _Just





-- * Mark

data Dataset d = Dataset {
  _datasetRows :: [d]
  , _datasetName :: String
  } deriving (Eq, Ord, Show, G.Generic)
makeLenses ''Dataset

-- -- | Based on 'sopFieldNames': two Datasets are equal iff they share the same set of column names.
-- --
-- -- This is different from requiring all datasets _content_ to be equal, in that e.g. it doesn't entail traversing all dataset rows 
-- instance (G.Generic d, GDatatypeInfo d) => Eq (Dataset d) where
--   Dataset (ds1:_) == Dataset (ds2:_) = fields ds1 == fields ds2
--   _ == _ = False

hasField :: (G.Generic d, GDatatypeInfo d) => String -> Dataset d -> Bool
hasField df (Dataset d _) = df `elem` fields (head d)

fields :: (G.Generic d, GDatatypeInfo d) => d -> [String]
fields = fold . sopFieldNames

data SymbolShape = Circle | Cross deriving (Eq, Show, Ord, G.Generic)
data MarkType = Rect | RectC | Symbol { _markSymbolShape :: SymbolShape } deriving (Eq, Show, Ord)
data ColFeatureTy = MarkFillCol | StrokeCol deriving (Eq, Ord, Show)

-- color features of a mark
newtype ColFeatures x = ColFeatures { _cfMap :: M.Map ColFeatureTy (ColFeature x) } deriving (Eq, Ord, Show)
makeLenses ''ColFeatures

markFillCol, markStrokeCol :: Traversal' (ColFeatures x) (ColFeature x)
markFillCol = cfMap . at MarkFillCol . _Just
markStrokeCol = cfMap . at StrokeCol . _Just


colFeatures :: ColFeatures x
colFeatures = ColFeatures M.empty

-- | Each mark is associated with a few (>= 0) geometry and colour features
data Mark x = Mark {
    _mark :: MarkType
  , _geom :: GeomFeatures x -- ^ Geometry features
  , _col :: ColFeatures x -- ^ Colour features
  } deriving (Eq, Ord, Show, G.Generic)
makeLenses ''Mark

circle, rectC :: Mark x
circle = Mark (Symbol Circle) geomFeatures colFeatures
rectC = Mark RectC geomFeatures colFeatures

x, y, x2, y2, width, height :: Traversal' (Mark x) (ScalarFeature x)
x = geom . gfX
y = geom . gfY
x2 = geom . gfX2
y2 = geom . gfY2
width = geom . gfWidth
height = geom . gfHeight

fill, stroke :: Traversal' (Mark x) (ColFeature x)
fill = col . markFillCol
stroke = col . markStrokeCol






newtype Marks x = Marks {
    _marks :: [Mark x]
    }
makeLenses ''Marks

noMarks :: Marks x
noMarks = Marks []








data Channel d a = Channel {
    _chDataset :: Dataset d
  , _chScale :: Scale a
  , _chMark :: Mark a } deriving (Eq, Show)
makeLenses ''Channel

scaleFromData :: String -- ^ name of the new scale
              -> Dataset d
              -> ScaleType
              -> String -- ^ name of the data field to be used
              -> Range a
              -> Scale a
scaleFromData sn (Dataset _ dn) sclTy sField rg = scl
  where
    scl = Scale (Just sn) sclTy sdom rg
    sdom = DomainData dn sField






-- data Node d a =
--      NdData {
--        _ndDataset :: Dataset d
--                }
--    | NdScale {
--        _ndScale :: Scale a
--            }
--    | NdMark {
--        _ndMark :: Mark a
--             }
--    deriving (Eq, Ord, Show, G.Generic)
-- makeLenses ''Node



-- -- newChannel :: (Ord d, Ord a) =>
-- --               Mark a -> Scale a -> Dataset d -> AdjacencyMap (Node d a)
-- -- newChannel mk scl d = newDataset d `connect` (newScale scl' `connect` newMark mk)
-- --   where
-- --     scl' = scl & scaleName .~ Nothing

-- newScale :: Scale a -> AdjacencyMap (Node d a)
-- newScale scl = vertex $ NdScale scl

-- newDataset :: Dataset d -> AdjacencyMap (Node d a)
-- newDataset d = vertex $ NdData d 

-- newMark :: Mark a -> AdjacencyMap (Node d a)
-- newMark mk = vertex $ NdMark mk




