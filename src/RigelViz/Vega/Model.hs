{-# language LambdaCase #-}
{-# language TemplateHaskell #-}
{-# language DeriveGeneric #-}
{-# language DeriveFunctor #-}
{-# language GeneralizedNewtypeDeriving #-}
module RigelViz.Vega.Model where



import qualified GHC.Generics as G (Generic(..))

-- aeson
import qualified Data.Aeson as A (ToJSON(..))
-- colour
import qualified Data.Colour as C
import qualified Data.Colour.SRGB as C (sRGB24show)
-- containers
import qualified Data.Map as M (Map, fromList, insert, empty)
-- microlens

-- microlens-ghc   -- traversals for 'containers' et al.
import Lens.Micro.GHC (Lens, Lens', Traversal, Traversal', (&))
import Lens.Micro.GHC ((.~), (?~)) -- setters
import Lens.Micro.GHC ((%~)) -- modifiers
import Lens.Micro.GHC ((^.)) -- getters
import Lens.Micro.GHC (traverseOf, traversed, _Just) -- traversals
-- microlens-mtl
import Lens.Micro.Mtl ((.=), (?=)) -- setters
import Lens.Micro.Mtl ((%=), zoom) -- modifiers
import Lens.Micro.Mtl (use) -- getters
-- microlens-th
import Lens.Micro.TH
-- mtl
import Control.Monad.State.Class (MonadState(..), gets)
import Control.Monad.State (State(..), runState, StateT(..), runStateT)

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
  | RangeBounds a a
  deriving (Eq, Show, G.Generic, Functor)
instance A.ToJSON a => A.ToJSON (Range a)

data ColourRange = Plasma | Category20 | BlueOrange deriving (Show, G.Generic)

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

scale :: ScaleType -> Domain a -> Range a -> Scale a
scale = Scale Nothing 

-- -- | assign a name to a scale
-- nameScale :: String -> Scale a -> Scale a
-- nameScale n = scaleName ?~ n

nameDomainDataRef :: Traversal' (Scale a) (Maybe String)
nameDomainDataRef = scaleDomain . domainDataRef 

nameDomainDataField :: Traversal' (Scale a) String
nameDomainDataField = scaleDomain . domainDataField




-- ** Scale (colour)

data ColourScale = ColourScale {
    colourScaleName :: Maybe String
  , colourScaleType :: ColourScaleType
                               } deriving (Eq, Show, G.Generic)

type Col = C.Colour Double

-- instance A.ToJSON (C.Colour a) where

data ColourScaleType =
  ColLinear { colLinearScale :: String }
  | ColOrdinal [Col]
  deriving (Eq, Show, G.Generic)



-- ** Features : can be either constant or referring to a scale

-- | A ScalarFeature could be a coordinate or a size annotation, either constant or tied to a data scale
data ScalarFeature x =
    GFConst { _sfConst :: x } -- ^ constant
  | GFFromScale { _sfScale :: Scale x } -- ^ derived from a 'scale'
  deriving (Eq, Show, G.Generic, Functor)
makeLenses ''ScalarFeature
instance A.ToJSON x => A.ToJSON (ScalarFeature x) where

-- setSfConst k = sfConst .~ k

nameScaleDomainDataRef :: Traversal' (ScalarFeature x) (Maybe String)
nameScaleDomainDataRef = sfScale . nameDomainDataRef

nameScaleDomainDataField :: Traversal' (ScalarFeature x) String
nameScaleDomainDataField = sfScale . nameDomainDataField

-- | A ColFeature is a colour annotation, either constant or tied to a data scale
data ColFeature =
    CFConst Col -- ^ constant
  | CFFromScale ColourScale -- ^ derived from a 'scale'
  deriving (Eq, Show, G.Generic)
-- instance A.ToJSON ColFeature where  -- FIXME




-- * Geometry features

data GeomFeatureTy = X | Y | X2 | Y2 | Width | Height deriving (Eq, Show, Ord)
newtype GeomFeatures x = GeomFeatures {
  _gfMap :: M.Map GeomFeatureTy (ScalarFeature x) } deriving (Eq, Show, Functor)
makeLenses ''GeomFeatures

geomFeatures :: GeomFeatures a
geomFeatures = GeomFeatures M.empty

insertGf k v = gfMap %~ M.insert k v


-- data GeomFeatures x = GeomFeatures {
--   _gfX :: Maybe (ScalarFeature x)
--   , _gfY :: Maybe (ScalarFeature x)
--   , _gfX2 :: Maybe (ScalarFeature x)
--   , _gfY2 :: Maybe (ScalarFeature x)
--   , _gfWidth :: Maybe (ScalarFeature x)
--   , _gfHeight :: Maybe (ScalarFeature x)
--   } deriving (Eq, Show, G.Generic, Functor)
-- makeLenses ''GeomFeatures

-- geomFeatures :: GeomFeatures a
-- geomFeatures = GeomFeatures Nothing Nothing Nothing Nothing Nothing Nothing

-- x :: Traversal' (GeomFeatures x) (ScalarFeature x)
-- x = gfX . _Just

-- xScale :: Traversal' (GeomFeatures x) (Scale x)
-- xScale = x . sfScale


-- * Mark

data SymbolShape = Circle | Cross deriving (Eq, Show, Ord, G.Generic)
data MarkType = Rect | RectC | Symbol { _markSymbolShape :: SymbolShape } deriving (Eq, Show, Ord)
data ColFeatureTy = MarkFillCol | StrokeCol deriving (Eq, Show, Ord)

-- color features of a mark
newtype ColFeatures = ColFeatures { _cfMap :: M.Map ColFeatureTy ColFeature } deriving (Eq, Show)
makeLenses ''ColFeatures
colFeatures :: ColFeatures
colFeatures = ColFeatures M.empty

-- | Each mark is associated with a few (>= 0) geometry and colour features
data Mark x = Mark {
    _mark :: MarkType
  , _geom :: GeomFeatures x -- ^ Geometry features
  , _col :: ColFeatures -- ^ Colour features
  } deriving (Eq, Show, G.Generic, Functor)
makeLenses ''Mark

circle :: Mark x
circle = Mark (Symbol Circle) geomFeatures colFeatures


{- INTUITION :

the initial state is specified by the user, e.g. as the following plot declaration

scatter :: Scale x -> Scale x -> Mark x
scatter xs ys =
  circle &
    geom %~ xScale xs &
    geom %~ yScale ys

the compiler then populates all scale, axis, dataset names accordingly, while performing checks on the input value

-}







-- newtype Marks x = Marks {
--     _mfGeomMap :: [(MarkType, MarkFeatures x)]
--                                    }
-- makeLenses ''Marks


-- insertMf k k2 =
--   mfGeomMap %~ M.insert k mks
--     where
--       mks = (markFeaturesGeom %~ x)
  

-- data MarkType x =
--   Rect {
--     _markRectY2 :: Maybe (ScalarFeature x)
--   , _markRectWidth :: Maybe (ScalarFeature x)
--        }
--   | RectC {
--     _markRectCWidth :: Maybe (ScalarFeature x)
--   , _markRectCHeight :: Maybe (ScalarFeature x)
--           }
--   | Symbol {
--     _markSymbolShape :: SymbolShape
--   , _markSymbolSize :: Maybe (ScalarFeature x)
--            } deriving (Eq, Show, G.Generic)
-- makeLenses ''MarkType

-- -- scatter = mk & markRectY2 . markType ?~ GFConst 5
-- --   where
-- --     mk = Symbol Circle Nothing

-- data Mark x = Mark {
--     _markType :: MarkType x
--   , _x :: Maybe (ScalarFeature x) -- ^ the meaning of field X depends on mark type
--   , _y :: Maybe (ScalarFeature x) -- ^ the meaning of field Y depends on mark type
--   , _fill :: Maybe ColFeature
--   , _fillOpacity :: Maybe (ScalarFeature x)
--                    } deriving (Eq, Show, G.Generic)
-- makeLenses ''Mark


-- -- scatter = mk & x ?~ 
-- --   where
-- --     mty = Symbol Circle Nothing
-- --     mk = Mark mty Nothing Nothing Nothing Nothing




-- -- | Rectangle mark, given its mid-base edge coordinates
-- data Rect x = Rect {
--     _rectX :: Maybe (ScalarFeature x)
--   , _rectY :: Maybe (ScalarFeature x)
--   , _rectY2 :: Maybe (ScalarFeature x)
--   , _rectWidth :: Maybe (ScalarFeature x)
--   , _rectFill :: Maybe ColFeature
--   , _rectFillOpacity :: Maybe (ScalarFeature x)
--                  } deriving (G.Generic)
-- makeLenses ''Rect

-- -- | Rectangle mark, given its center coordinates
-- data RectC x = RectC {
--     _rectCX :: Maybe (ScalarFeature x)
--   , _rectCY :: Maybe (ScalarFeature x)
--   , _rectCWidth :: Maybe (ScalarFeature x)
--   , _rectCHeight :: Maybe (ScalarFeature x)  
--   , _rectCFill :: Maybe ColFeature
--   , _rectCFillOpacity :: Maybe (ScalarFeature x)
--                  }
-- makeLenses ''RectC

-- -- | Symbol mark
-- data Symbol x = Symbol {
--     _symbolX :: Maybe (ScalarFeature x)
--   , _symbolY :: Maybe (ScalarFeature x)
--   , _symbolShape :: Maybe SymbolShape
--   , _symbolFill :: Maybe ColFeature
--   , _symbolFillOpacity :: Maybe (ScalarFeature x)
--   , _symbolSize :: Maybe (ScalarFeature x)
--                        }
-- makeLenses ''Symbol


-- -- class HasCoordinates t where
-- --   x :: Lens' t (Maybe (ScalarFeature x))

