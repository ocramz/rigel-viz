{-# language DeriveFunctor #-}
{-# language DeriveGeneric #-}
{-# language TemplateHaskell #-}

-- | Common types for Vega.Model

module RigelViz.Vega.Model.Common where

import qualified GHC.Generics as G (Generic(..))

-- aeson
import qualified Data.Aeson as A (ToJSON(..), encode, Value, object)
import Data.Aeson ((.=))
-- colour
import qualified Data.Colour as C
import qualified Data.Colour.SRGB as C (RGB(..), toSRGB, sRGB24show)
-- microlens-th
import Lens.Micro.TH (makeLenses)

-- ** Range
  
data Range a = RangeWidth
             | RangeHeight
             | RangeBounds { _rangeBoundsMin :: a, _rangeBoundsMax :: a}
             deriving (Eq, Ord, Show, G.Generic, Functor)
instance A.ToJSON a => A.ToJSON (Range a)
makeLenses ''Range

-- ** Scale

data LinearScaleTy = Lin
                   | Log
                   | Pow
                   deriving (Eq, Ord, Show, G.Generic)
data DiscreteScaleTy = Ordinal
                     | Band
                     | Point
                     deriving (Eq, Ord, Show, G.Generic)
instance A.ToJSON LinearScaleTy where
instance A.ToJSON DiscreteScaleTy where

data ScaleType = LinearScale LinearScaleTy
               | DiscreteScale DiscreteScaleTy -- ^ discrete, ordered values
               deriving (Eq, Ord, Show, G.Generic)
instance A.ToJSON ScaleType where



-- ** Scale (colour)

data ColourPalette = Plasma
                   | Category20
                   | BlueOrange
                   deriving (Eq, Ord, Show, G.Generic)

newtype Col = Col (C.Colour Double) deriving (Eq, Show)
-- | RGB colours are really a lattice, not a totally ordered field (FIXME)
instance Ord Col where
  (Col c1) <= (Col c2) = colNorm c1 <= colNorm c2

colNorm :: (Floating a, Ord a) => C.Colour a -> a
colNorm c = sqrt $ sum $ zipWith (*) v v where
  v = [r, g, b]
  (C.RGB r g b) = C.toSRGB c


data ColourScaleTy = ColLinear { _colLinearScaleRange :: ColourPalette }
                   | ColOrdinal { _colOrdinalScaleRange :: [Col] }
                   deriving (Eq, Ord, Show, G.Generic)
makeLenses ''ColourScaleTy


-- * Geometry features

data GeomFeatureTy = X
                   | Y
                   | X2
                   | Y2
                   | Width
                   | Height
                   deriving (Eq, Show, Ord)


-- * Mark

data SymbolShape = Circle | Cross | Triangle | Arrow | Wedge deriving (Eq, Show, Ord, G.Generic)

data MarkTy = Line
            | Rect
            | RectC 
            | Symbol { _markSymbolShape :: SymbolShape }
            | Text { _markText :: String }
            deriving (Eq, Show, Ord)

data ColFeatureTy = MarkFillCol | StrokeCol deriving (Eq, Ord, Show)
