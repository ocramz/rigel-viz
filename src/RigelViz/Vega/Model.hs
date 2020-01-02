{-# language LambdaCase #-}
{-# language TemplateHaskell #-}
{-# language DeriveGeneric #-}
module RigelViz.Vega.Model where

import qualified GHC.Generics as G (Generic(..))

-- aeson
import qualified Data.Aeson as A (ToJSON(..))
-- colour
import qualified Data.Colour as C
import qualified Data.Colour.SRGB as C (sRGB24show)
-- microlens
import Lens.Micro (Lens', (&))
import Lens.Micro ((.~), (?~)) -- setters
import Lens.Micro ((^.)) -- getters
-- microlens-th
import Lens.Micro.TH

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






-- | A ScalarFeature could be a coordinate or a size annotation, either constant or tied to a data scale
data ScalarFeature x =
    GFConst x -- ^ constant
  | GFFromScale (Scale x) -- ^ derived from a 'scale'
  deriving (Eq, Show, G.Generic)
instance A.ToJSON x => A.ToJSON (ScalarFeature x) where

-- | A ColFeature is a colour annotation, either constant or tied to a data scale
data ColFeature =
    CFConst Col -- ^ constant
  | CFFromScale ColourScale -- ^ derived from a 'scale'
  deriving (Eq, Show, G.Generic)
-- instance A.ToJSON ColFeature where  -- FIXME


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
  | DomainDataRef {
      domainDataField :: String -- ^ field name (dataset name comes from the context)
      } deriving (Eq, Show, G.Generic)
instance A.ToJSON a => A.ToJSON (Domain a) where

-- ** Range
  
data Range a =
  RangeWidth
  | RangeHeight
  | RangeBounds a a
  deriving (Eq, Show, G.Generic)
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
                   } deriving (Eq, Show, G.Generic)
makeLenses Scale
instance A.ToJSON a => A.ToJSON (Scale a) where

scale :: ScaleType -> Domain a -> Range a -> Scale a
scale = Scale Nothing 




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

-- -- * Mark


data SymbolShape = Circle | Cross deriving (Eq, Show, G.Generic)

data MarkType x =
  Rect {
    _markRectY2 :: Maybe (ScalarFeature x)
  , _markRectWidth :: Maybe (ScalarFeature x)
       }
  | RectC {
    _markRectCWidth :: Maybe (ScalarFeature x)
  , _markRectCHeight :: Maybe (ScalarFeature x)
          }
  | Symbol {
    _markSymbolShape :: SymbolShape
  , _markSymbolSize :: Maybe (ScalarFeature x)
           } deriving (Eq, Show, G.Generic)
makeLenses ''MarkType

-- scatter = mk & markRectY2 . markType ?~ GFConst 5
--   where
--     mk = Symbol Circle Nothing

data Mark x = Mark {
    _markType :: MarkType x
  , _x :: Maybe (ScalarFeature x) -- ^ the meaning of field X depends on mark type
  , _y :: Maybe (ScalarFeature x) -- ^ the meaning of field Y depends on mark type
  , _fill :: Maybe ColFeature
  , _fillOpacity :: Maybe (ScalarFeature x)
                   } deriving (Eq, Show, G.Generic)
makeLenses ''Mark


-- scatter = mk & x ?~ 
--   where
--     mty = Symbol Circle Nothing
--     mk = Mark mty Nothing Nothing Nothing Nothing




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

