{-# language LambdaCase #-}
{-# language TemplateHaskell #-}
{-# language DeriveGeneric #-}
{-# language DeriveFunctor #-}
{-# language GeneralizedNewtypeDeriving, DeriveTraversable #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module RigelViz.Vega.Model where

import Control.Applicative (liftA2)
import qualified GHC.Generics as G (Generic(..))

-- aeson
import qualified Data.Aeson as A (ToJSON(..))
-- colour
import qualified Data.Colour as C
import qualified Data.Colour.SRGB as C (sRGB24show)
-- containers
import qualified Data.Map as M (Map, fromList, insert, empty, lookup)
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

import RigelViz.Vega.Types
import RigelViz.Vega.Generics (sopFieldNames)



-- data Plot d =
--   ScatterPlot {
--     spData :: d
--     }
--   | LinePlot {
--     lpData :: d
--              }
--   | HeatmapPlot {
--     hpData :: d
--     } deriving (Eq, Show)

-- renderPlot = \case
--   ScatterPlot d -> undefined




{- plot composition operator ideas :

a <=> b = undefined
u <||> v = undefined 
-}



-- -- * Channel

-- data Channel d = Channel {
--     channelDataset :: d
--   , channelField :: String
--   }












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
    DomainValues [a]
  | DomainData {
        _domainDataRef :: Maybe String
      , _domainDataField :: String 
      } deriving (Eq, Show, G.Generic, Functor)
makeLenses ''Domain
instance A.ToJSON a => A.ToJSON (Domain a) where

-- ** Range
  
data Range a =
  RangeWidth
  | RangeHeight
  | RangeBounds { _rangeBoundsMin :: a, _rangeBoundsMax :: a}
  deriving (Eq, Show, G.Generic, Functor)
instance A.ToJSON a => A.ToJSON (Range a)
makeLenses ''Range



-- ** Scale

data ScaleType =
  Linear
  | Ordinal -- ^ discrete, ordered values
  deriving (Eq, Show, G.Generic)
instance A.ToJSON ScaleType where

data Scale a = Scale {
    _scaleName :: Maybe String -- initially Nothing
  , _scaleType :: ScaleType
  , _scaleDomain :: Domain a
  , _scaleRange :: Range a 
                   } deriving (Eq, Show, G.Generic, Functor)
makeLenses ''Scale
instance A.ToJSON a => A.ToJSON (Scale a) where

-- scale :: ScaleType -> Domain a -> Range a -> Scale a
-- scale = Scale Nothing 

-- | Name of the data reference used by this scale
nameDomainDataRef :: Traversal' (Scale a) (Maybe String)
nameDomainDataRef = scaleDomain . domainDataRef 

-- | Name of the data field used by this scale
nameDomainDataField :: Traversal' (Scale a) String
nameDomainDataField = scaleDomain . domainDataField




-- ** Scale (colour)

data ColourRange = Plasma | Category20 | BlueOrange deriving (Eq, Show, G.Generic)

type Col = C.Colour Double

data ColourScaleType =
  ColLinear { _colLinearScaleName :: String }
  | ColOrdinal { _colOrdinalColours :: [Col] }
  deriving (Eq, Show, G.Generic)

data ColourScale a = ColourScale {
    _colourScaleName :: Maybe String
  , _colourScaleType :: ColourScaleType
  , _colourScaleDomain :: Domain a
  , _colourScaleRange :: ColourRange
  } deriving (Eq, Show, G.Generic)
makeLenses ''ColourScale

-- instance A.ToJSON (C.Colour a) where





-- ** Features : can be either constant or referring to a scale

-- | A ScalarFeature could be a coordinate or a size annotation, either constant or tied to a data scale
data ScalarFeature x =
    GFConst { _sfConst :: x } -- ^ constant
  | GFFromScale { _sfScale :: Scale x } -- ^ derived from a 'scale'
  deriving (Eq, Show, G.Generic, Functor)
makeLenses ''ScalarFeature
instance A.ToJSON x => A.ToJSON (ScalarFeature x) where

-- sfConst :: Traversal' (ScalarFeature x) x
  
-- sfScale :: Traversal' (ScalarFeature x) (Scale x)



nameScaleDomainDataRef :: Traversal' (ScalarFeature x) (Maybe String)
nameScaleDomainDataRef = sfScale . nameDomainDataRef

nameScaleDomainDataField :: Traversal' (ScalarFeature x) String
nameScaleDomainDataField = sfScale . nameDomainDataField

-- | A ColFeature is a colour annotation, either constant or tied to a data scale
data ColFeature x =
    CFConst { _cfConst :: Col} -- ^ constant
  | CFFromScale { _cfScale :: ColourScale x } -- ^ derived from a 'scale'
  deriving (Eq, Show, G.Generic)
makeLenses ''ColFeature

-- instance A.ToJSON ColFeature where  -- FIXME




-- * Geometry features

data GeomFeatureTy = X | Y | X2 | Y2 | Width | Height deriving (Eq, Show, Ord)
newtype GeomFeatures x = GeomFeatures {
  _gfMap :: M.Map GeomFeatureTy (ScalarFeature x) } deriving (Eq, Show, Functor)
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



-- lookupGf k (GeomFeatures gfm) = M.lookup k gfm

-- x = lookupGf X

-- insertGf k v = gfMap %~ M.insert k v

-- fromListGf kvs = gfMap .~ M.fromList kvs


mapAccumM :: (Traversable t, Monad m) =>
             (a -> s -> m (b, s))
          -> t a
          -> s
          -> m (t b, s)
mapAccumM f = runStateT . traverse (StateT . f)





-- * Mark

data SymbolShape = Circle | Cross deriving (Eq, Show, Ord, G.Generic)
data MarkType = Rect | RectC | Symbol { _markSymbolShape :: SymbolShape } deriving (Eq, Show, Ord)
data ColFeatureTy = MarkFillCol | StrokeCol deriving (Eq, Show, Ord)

-- color features of a mark
newtype ColFeatures x = ColFeatures { _cfMap :: M.Map ColFeatureTy (ColFeature x) } deriving (Eq, Show)
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
  } deriving (Eq, Show, G.Generic)
makeLenses ''Mark

circle :: Mark x
circle = Mark (Symbol Circle) geomFeatures colFeatures

x, y :: Traversal' (Mark x) (ScalarFeature x)
x = geom . gfX
y = geom . gfY

fill, stroke :: Traversal' (Mark x) (ColFeature x)
fill = col . markFillCol
stroke = col . markStrokeCol




{- INTUITION :

the initial state is specified by the user, e.g. as the following plot declaration

scatter :: Scale x -> Scale x -> Mark x
scatter xs ys =
  circle &
    geom %~ xScale xs &
    geom %~ yScale ys

the compiler then populates all scale, axis, dataset names accordingly, while performing checks on the input value

-}







newtype Marks x = Marks {
    _marks :: [Mark x]
    }
makeLenses ''Marks

noMarks :: Marks x
noMarks = Marks []


-- addMark mk = marks %~ (mk :)




