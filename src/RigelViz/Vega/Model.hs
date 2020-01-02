{-# language LambdaCase #-}
module RigelViz.Vega.Model where

-- colour
import qualified Data.Colour as C
import qualified Data.Colour.SRGB as C (sRGB24show)

import RigelViz.Vega.Types
import RigelViz.Vega.Generics (sopFieldNames)



data Plot d =
  ScatterPlot {
    spData :: d
    }
  | LinePlot {
    lpData :: d
             }
  | HeatmapPlot {
    hpData :: d
    } deriving (Eq, Show)

renderPlot = \case
  ScatterPlot d -> undefined



-- * Channel

data Channel d = Channel {
    channelDataset :: d
  , channelField :: String
  }



-- * Mark

data Mark d = Mark {
    markChannel :: Channel d
  -- , markGlyph :: Glyph
                   }

data Glyph x =
    GlRect {
        glRectX :: ScalarFeature x -- x
      , glRectY :: ScalarFeature x -- y
      , glRectY2 :: ScalarFeature x -- y2
      , glRectWidth :: ScalarFeature x -- width
      , glRectFill :: ColFeature  -- fill
      , glRectFillOpacity :: ScalarFeature x -- fillOpacity
         }
  | GlRectC {
        glRectCXc :: ScalarFeature x -- xc
      , glRectCYc :: ScalarFeature x -- yc
      , glRectCWidth :: ScalarFeature x -- width
      , glRectCHeight :: ScalarFeature x -- height
      , glRectCFill :: ColFeature  -- fill
      , glRectCFillOpacity :: ScalarFeature x -- fillOpacity
         }
  | GlSymbol {
        glSymX :: ScalarFeature x -- x
      , glSymY :: ScalarFeature x -- y
      , glSymShape :: SymbolShape -- shape
      , glSymFill :: ColFeature -- fill
      , glSymSize :: ScalarFeature x -- size
           }

data SymbolShape = Circle | Cross

data ScalarFeature x =
    GFConst x -- ^ constant
  | GFFromScale Scale -- ^ derived from a 'scale'

data ColFeature =
    CFConst Col -- ^ constant
  | CFFromScale ColourScale -- ^ derived from a 'scale'


-- * Scale

data Range

data Scale = Scale {
    scaleName :: Maybe String -- initially Nothing
  , scaleType :: ScaleType
                   }
             
data ScaleType =
  Linear
  | Ordinal -- ^ discrete, ordered values
  deriving (Eq, Show)

data ColourScale = ColourScale {
  colourScaleName :: Maybe String
  , colourScaleType :: ColourScaleType
                               }

type Col = C.Colour Double

data ColourScaleType =
  ColLinear { colLinearScale :: String }
  | ColOrdinal [Col]
