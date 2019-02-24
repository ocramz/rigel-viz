{-# language OverloadedStrings, DeriveGeneric, LambdaCase #-}
module RigelViz (
  vegaLiteSpec, VLSpec,
  -- * Data sources
  Data(..),
  -- * Layer
  layer,  LayerMetadata,
  -- * Mark 
  MarkType(..),
  -- * Data encoding options
  EncSet, posEnc, Pos(..), colourEnc, colour, sizeEnc, EncodingType(..)) where

import qualified Data.Set as S
import GHC.Generics (Generic(..))
import qualified Data.Aeson as A
import Data.Aeson ((.=))
import Data.Char (toLower)
import qualified Data.Text as T
-- import qualified Data.Text.Encoding as T (decodeUtf8)
-- import qualified Data.ByteString as BS hiding (pack)
-- import qualified Data.ByteString.Lazy.Char8 as BS (unpack)
-- import qualified Data.ByteString.Lazy as LBS (toStrict)
-- import Data.Monoid
import qualified Data.Colour as C
import qualified Data.Colour.SRGB as C (sRGB24show)


-- toJSONText :: A.ToJSON a => a -> T.Text
-- toJSONText e = T.decodeUtf8 $ LBS.toStrict $ A.encode e

-- | The current schema version is 3
schema :: Int -> String
schema vn = mconcat ["https://vega.github.io/schema/vega-lite/v", show vn,".json"]

-- | Create a @vega-lite@ spec
vegaLiteSpec ::
     Int  -- ^ Plot width
  -> Int  -- ^ Plot height
  -> Data a -- ^ Data source
  -> [LayerMetadata]  -- ^ Plot layer encoding metadata
  -> VLSpec a
vegaLiteSpec = VLSpec

-- | Specification of a vega-lite plot
--
-- A 'VLSpec' can be encoded into a JSON blob via its 'A.ToJSON' instance.
data VLSpec a = VLSpec {
    vlsWidth :: Int
  , vlsHeight :: Int
  , vlsData :: Data a
  , vlsView :: [LayerMetadata]
  } deriving (Eq, Show, Generic)
instance A.ToJSON a => A.ToJSON (VLSpec a) where
  toJSON (VLSpec w h dats lms) = A.object $ ("layer" .= map A.toJSON lms) : defs
    where
      defs = [
        "$schema" .= schema 3
        , "width" .= w
        , "height" .= h
        , "data" .= dats
        ]

-- | Data source
data Data a =
    DataJSON [a]  -- ^ Data row type must have a 'A.ToJSON' instance
  | DataURI String -- ^ URI or filepath of dataset
  deriving (Eq, Show, Generic)
instance A.ToJSON a => A.ToJSON (Data a) where
  toJSON = \case
    DataJSON vs -> A.object ["values" .= vs]
    DataURI u   -> A.object ["url" .= u]

-- | Layer metadata

data LayerMetadata = LayerMD Mark EncSet deriving (Eq, Show, Generic)
instance A.ToJSON LayerMetadata where
  toJSON (LayerMD m e) = A.object ["mark" .= m, "encoding" .= e]

-- | Declare a plot layer
layer :: MarkType -> EncSet -> LayerMetadata
layer m es = LayerMD (Mark m) es

-- | Set of channel encoding options.
--
-- Options are created with 'posEnc', 'colourEnc' / 'colour', 'sizeEnc' and can be added to an 'EncSet' via its 'Semigroup' instance
newtype EncSet  = EncSet (S.Set Encoding) deriving (Eq, Show, Generic)
instance A.ToJSON EncSet where
  toJSON (EncSet es) = A.object $ S.foldr insf [] es where
    insf el acc = case el of
      EsPos p emd  -> (showPos p .= emd) : acc
      EsColour emd -> ("color" .= emd) : acc
      EsSize emd   -> ("size" .= emd) : acc      

singleton :: Encoding -> EncSet
singleton = EncSet . S.singleton

instance Semigroup EncSet where
  (EncSet s1) <> (EncSet s2) = EncSet $ s1 <> s2

-- | Position encoding
posEnc :: Pos
  -> T.Text   -- ^ Field in the data source
  -> EncodingType
  -> EncSet
posEnc p f t = singleton $ EsPos p $ EncMD f t

-- | Fixed colour
colour :: C.Colour Double -> EncSet
colour c = singleton $ EsColour $ ColourFixed c

-- | Colour encoding
colourEnc :: T.Text -- ^ Field in the data source
  -> EncodingType
  -> EncSet
colourEnc f t = singleton $ EsColour $ ColourEnc $ EncMD f t

-- | Size encoding
sizeEnc :: T.Text -- ^ Field in the data source
  -> EncodingType
  -> EncSet
sizeEnc f t = singleton $ EsSize $ SizeEnc $ EncMD f t

-- | Encoding channels for a layer
data Encoding =
    EsPos Pos EncMetadata
  | EsColour Colour
  | EsSize Size
  deriving (Eq, Show, Ord, Generic)

-- | Position encoding alternatives
data Pos = X | Y | X2 | Y2 deriving (Eq, Ord, Show)
showPos :: Pos -> T.Text
showPos p = T.pack $ map toLower $ show p

newtype Mark = Mark { mType :: MarkType } deriving (Eq, Show, Generic)
instance A.ToJSON Mark where
  toJSON (Mark mty) = A.object ["type" .= mty]

-- | Mark type alternatives
data MarkType =
    MPoint  -- ^ "point"
  | MCircle -- ^ "circle"
  | MRect   -- ^ "rect"
  | MBar    -- ^ "bar"
  | MArea   -- ^ "area"
  | MRule   -- ^ "rule"
  deriving (Eq, Show, Generic)
instance A.ToJSON MarkType where
  toJSON = \case
    MPoint  -> "point"
    MCircle -> "circle"
    MRect   -> "rect"
    MBar    -> "bar"
    MArea   -> "area"
    MRule   -> "rule"

data EncMetadata = EncMD { encField :: T.Text, emType :: EncodingType } deriving (Eq, Show, Ord, Generic)
instance A.ToJSON EncMetadata where
  toJSON (EncMD f t) = A.object [ "field" .= f, "type" .= t]

-- | "colour" encoding channel metadata
data Colour =
    ColourFixed (C.Colour Double)
  | ColourEnc EncMetadata
  deriving (Eq, Show, Generic)
instance Ord Colour where
  ColourFixed c1 <= ColourFixed c2 = C.sRGB24show c1 <= C.sRGB24show c2 -- ewwwwwww
  ColourEnc e1 <= ColourEnc e2 = e1 <= e2
  _ <= _ = False
instance A.ToJSON Colour where
  toJSON = \case
    ColourFixed c -> A.object ["value" .= C.sRGB24show c]
    ColourEnc emd -> A.toJSON emd

-- | "size" encoding channel metadata
data Size =
    SizeFixed Double
  | SizeEnc EncMetadata
  deriving (Eq, Show, Ord, Generic)
instance A.ToJSON Size where
  toJSON = \case
    SizeFixed sz -> A.object ["value" .= sz]
    SizeEnc emd  -> A.toJSON emd

-- | encoding type
data EncodingType = Nominal | Quantitative | Temporal | Ordinal deriving (Eq, Show, Ord, Generic)
instance A.ToJSON EncodingType where
  toJSON = \case
    Nominal      -> "nominal"
    Quantitative -> "quantitative"
    Temporal     -> "temporal"
    Ordinal      -> "ordinal"

{-

{
  "$schema": "https://vega.github.io/schema/vega-lite/v3.json",
  "data": {"url": "data/movies.json"},
  "transform": [{
    "filter": {"and": [
      {"field": "IMDB_Rating", "valid": true},
      {"field": "Rotten_Tomatoes_Rating", "valid": true}
    ]}
  }],
  "mark": "rect",
  "width": 300,
  "height": 200,
  "encoding": {
    "x": {
      "bin": {"maxbins":60},
      "field": "IMDB_Rating",
      "type": "quantitative"
    },
    "y": {
      "bin": {"maxbins": 40},
      "field": "Rotten_Tomatoes_Rating",
      "type": "quantitative"
    },
    "color": {
      "aggregate": "count",
      "type": "quantitative"
    }
  },
  "config": {
    "range": {
      "heatmap": {
        "scheme": "greenblue"
      }
    },
    "view": {
      "stroke": "transparent"
    }
  }
}

-}
