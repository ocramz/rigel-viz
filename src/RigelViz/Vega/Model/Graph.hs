{-# LANGUAGE FlexibleContexts #-}
{-# language LambdaCase #-}
{-# language TemplateHaskell #-}
{-# language DeriveGeneric #-}
{-# language DeriveFunctor #-}
{-# language GeneralizedNewtypeDeriving, DeriveTraversable #-}
{-# language OverloadedStrings #-}
{-# language GADTs #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module RigelViz.Vega.Model.Graph where

import Data.Foldable (Foldable(..))
import qualified GHC.Generics as G (Generic(..))

-- aeson
import qualified Data.Aeson as A (ToJSON(..), encode, Value, object)
import qualified Data.Aeson as A ((.=))
-- algebraic-graphs
import qualified Algebra.Graph as GA (Graph, connect, empty, overlay, vertex, edge)
import Algebra.Graph.AdjacencyMap (AdjacencyMap, adjacencyMap)
import qualified Algebra.Graph.ToGraph as GA (ToGraph(..))
-- bytestring
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS (ByteString, toStrict, fromStrict)
-- colour
import qualified Data.Colour as C
import qualified Data.Colour.SRGB as C (RGB(..), toSRGB, sRGB24show)
-- containers
import qualified Data.Map as M (Map, fromList, insert, fromListWith, empty, lookup)
import qualified Data.Set as S (Set)
-- generics-sop
import Generics.SOP.GGP (GDatatypeInfo)
-- microlens
import Lens.Micro (Lens, Lens', Traversal, Traversal', (&))
import Lens.Micro (ASetter, (.~), (?~)) -- setters
import Lens.Micro ((%~)) -- modifiers
import Lens.Micro (Getting, (^.), (^..)) -- getters
import Lens.Micro (traverseOf, traversed, _Just, over, each) -- traversals
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
-- text
import qualified Data.Text as T
-- unordered.containers
import qualified Data.HashMap.Strict as HM

import RigelViz.Vega.Generics (sopFieldNames)

import RigelViz.Vega.Model.Common (Range(..), ScaleTy(..), LinearScaleTy(..), DiscreteScaleTy(..), ScaleAxis(..))
import RigelViz.Vega.Model.Common (Col(..), ColourScaleTy(..), ColFeatureTy(..))
import RigelViz.Vega.Model.Common (GeomFeatureTy(..))
import RigelViz.Vega.Model.Common (SymbolShape(..), MarkTy(..))



{- INTUITION :

the initial state is specified by the user, e.g. as the following plot declaration

scatter :: Scale x -> Scale x -> Mark x
scatter xs ys =
  circle &
    geom %~ xScale xs &
    geom %~ yScale ys 

the compiler then populates all scale, axis, dataset names accordingly, while performing checks on the input value, eg with mapAccumM
-}









-- * Dataset

-- encoded datasets
newtype DS = DS {
  _unDS :: HM.HashMap T.Text [A.Value]
                } deriving (Show, G.Generic)
instance A.ToJSON DS where
  toJSON (DS dsm) = A.object [
    "values" A..= dsm 
                             ]

-- non-encoded dataset
data Dataset d = Dataset {
    _datasetRows :: [d]
  , _datasetName :: String
  } deriving (Show, G.Generic)
makeLenses ''Dataset

-- | NB : Two datasets are equal _iff their names_ are equal. This ensures that equality comparison doesn't traverse all rows but is also very brittle. 
instance Eq (Dataset d) where
  (Dataset _ dn1) == (Dataset _ dn2) = dn1 == dn2
-- | Ordering of datasets by name only
instance Ord (Dataset d) where
  (Dataset _ dn1) <= (Dataset _ dn2) = dn1 <= dn2



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



-- * Scale


-- ** Domain

data Domain a =
     DomainValues { _domainValues :: [a] }
   | DomainData {
        _domainDataRef :: String 
      , _domainDataField :: String
      } deriving (Eq, Ord, Show, G.Generic, Functor)
makeLenses ''Domain

-- -- | DomainData
-- domainData0 :: String -> Domain a
-- domainData0 = DomainData Nothing 

-- instance A.ToJSON a => A.ToJSON (Domain a) where

-- ** Range



-- ** Scale

data Scale a = Scale {
    _scaleName :: Maybe String -- initially Nothing
  , _scaleType :: ScaleTy
  , _scaleDomain :: Domain a
  , _scaleRange :: Range a 
  } deriving (Eq, Ord, Show, G.Generic, Functor)
makeLenses ''Scale
-- instance A.ToJSON a => A.ToJSON (Scale a) where

-- | Name of the data ref used by this scale
scaleDomainDataRef :: Traversal' (Scale a) String
scaleDomainDataRef = scaleDomain . domainDataRef
-- | Name of the data field used by this scale
scaleDomainDataField :: Traversal' (Scale a) String
scaleDomainDataField = scaleDomain . domainDataField


-- scale0 :: ScaleType -> Domain a -> Range a -> Scale a
-- scale0 = Scale Nothing

-- fromDataField :: ScaleType -> String -> Range a -> Scale a
-- fromDataField sty f = scale0 sty (domainData0 f)

-- linear :: String -> Range a -> Scale a
-- linear = fromDataField (LinearScale Lin)



-- -- | Linear scale
-- linearWidth, linearHeight :: String -- ^ Data field name
--                           -> Scale a
-- linearWidth f = fromDataField (LinearScale Lin) f RangeWidth
-- linearHeight f = fromDataField (LinearScale Lin) f RangeHeight





-- ** Scale (colour)

data ColourScale a = ColourScale {
    _colourScaleName :: Maybe String
  , _colourScaleType :: ColourScaleTy
  , _colourScaleDomain :: Domain a
  } deriving (Eq, Ord, Show, G.Generic)
makeLenses ''ColourScale

-- instance A.ToJSON (C.Colour a) where





-- ** Features : can be either constant or referring to a scale

-- | A ScalarFeature could be a coordinate or a size annotation, either constant or tied to a data scale
data ScalarFeature x = GFConst { _constant :: x } -- ^ constant
                     | GFFromScale {
                         _scale :: Scale x } -- ^ derived from a 'scale'
                     deriving (Eq, Ord, Show, G.Generic, Functor)
makeLenses ''ScalarFeature
-- instance A.ToJSON x => A.ToJSON (ScalarFeature x) where



-- nameScaleDomainDataRef :: Traversal' (ScalarFeature x) String
-- nameScaleDomainDataRef = sfScale . nameDomainDataRef

nameScaleDomainDataField :: Traversal' (ScalarFeature x) String
nameScaleDomainDataField = scale . scaleDomainDataField

-- | A ColFeature is a colour annotation, either constant or tied to a data scale
data ColFeature x = CFConst { _constantColour :: Col} -- ^ constant
                  | CFFromScale {
                      _colourScale :: ColourScale x } -- ^ derived from a 'scale'
                  deriving (Eq, Ord, Show, G.Generic)
makeLenses ''ColFeature

-- instance A.ToJSON ColFeature where  -- FIXME




-- * Geometry features


-- | Geometry features of a mark
newtype GeomFeatures x = GeomFeatures {
  _gfMap :: M.Map GeomFeatureTy (ScalarFeature x) } deriving (Eq, Ord, Show, Functor)
makeLenses ''GeomFeatures

-- | Empty geometry features map
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

-- | Color features of a mark
newtype ColFeatures x =
  ColFeatures { _cfMap :: M.Map ColFeatureTy (ColFeature x) } deriving (Eq, Ord, Show)
makeLenses ''ColFeatures

markFillCol, markStrokeCol :: Traversal' (ColFeatures x) (ColFeature x)
markFillCol = cfMap . at MarkFillCol . _Just
markStrokeCol = cfMap . at StrokeCol . _Just

-- | Empty colour features map
colFeatures :: ColFeatures x
colFeatures = ColFeatures M.empty

-- | Each mark is associated with a few (>= 0) geometry and colour features
data Mark x = Mark {
    _mark :: MarkTy
  , _markDataRef :: Maybe String -- initially Nothing
  , _geom :: GeomFeatures x -- ^ Geometry features
  , _col :: ColFeatures x -- ^ Colour features
  } deriving (Eq, Ord, Show, G.Generic)
makeLenses ''Mark

emptyMark :: MarkTy -> Mark x
emptyMark mty = Mark mty Nothing geomFeatures colFeatures

circle, rect :: Mark x
circle = emptyMark (Symbol Circle) 
rect = emptyMark Rect


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







-- newtype Marks x = Marks {
--     _marks :: [Mark x]
--     }
-- makeLenses ''Marks

-- noMarks :: Marks x
-- noMarks = Marks []






-- scaleFromData :: ScaleType
--               -> Range a 
--               -> String -- ^ name of the new scale
--               -> Maybe String -- ^ dataset reference
--               -> String -- ^ name of the data field to be used
--               -> Scale a
-- scaleFromData sclTy rg sn d sField = scl
--   where
--     scl = Scale (Just sn) sclTy sdom rg
--     sdom = DomainData d sField

-- -- linear, ordinal :: Range a -> String -> Dataset d -> String -> Scale a
-- linear = scaleFromData Linear
-- ordinal = scaleFromData Ordinal

-- -- linearWd :: String -> Dataset d -> String -> Scale a
-- linearWd = linear RangeWidth











mapAccumM :: (Traversable t, Monad m) =>
             (a -> s -> m (b, s)) -> t a -> s -> m (t b, s)
mapAccumM f = runStateT . traverse (StateT . f)




-- algebraic-graphs stuff

data Node d a =
     NdData {
       _ndDataset :: Dataset d
               }
   | NdScale {
       _ndScale :: Scale a
           }
   | NdMark {
       _ndMark :: Mark a
            }
   deriving (Eq, Ord, Show, G.Generic)
makeLenses ''Node

newtype Scales a = Scales { _scales :: M.Map ScaleAxis (Scale a)}
makeLenses ''Scales


data Supply d a = Supply {
    _dsSupply :: !Int
  , _scaleSupply :: !Int
  , _graph :: GA.Graph (Node d a) }
makeLenses ''Supply

supplyInit :: Supply d a
supplyInit = Supply 0 0 GA.empty

newtype G m d a b = G { unG :: StateT (Supply d a) m b }
  deriving (Functor, Applicative, Monad, MonadState (Supply d a))

runG :: G m d a b -> Supply d a -> m (b, Supply d a)
runG app = runStateT (unG app) 


datasetNode :: Dataset d -> GA.Graph (Node d a)
datasetNode = GA.vertex . NdData 











-- problem : edit all entries of 'b0 . a1' that are == Nothing into 'Just x' where x is taken sequentially from a list of strings

-- data A = A1 { _a1 :: Maybe String }
--   | A2 { _a2 :: Int }
--   deriving (Show)
-- makeLenses ''A

-- newtype B = B { _b :: M.Map String [A] } deriving (Show)
-- makeLenses ''B


-- b0 = B $ M.fromListWith (<>) [("a", ab0), ("b", ab1), ("a", ab2)] where
--   ab0 = [A1 Nothing]
--   ab1 = [A2 42]
--   ab2 = [A1 (Just "z")]

