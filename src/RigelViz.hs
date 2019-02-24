{-# language OverloadedStrings, DeriveGeneric, LambdaCase #-}
module RigelViz where

import qualified Data.Set as S
import GHC.Generics (Generic(..))
import qualified Data.Aeson as A
import Data.Aeson ((.=))
import qualified Data.Text as T
import qualified Data.Text.Encoding as T (decodeUtf8)
-- import qualified Data.ByteString as BS hiding (pack)
-- import qualified Data.ByteString.Lazy.Char8 as BS (unpack)
import qualified Data.ByteString.Lazy as LBS (toStrict)
-- import Data.Monoid
import qualified Data.Colour as C
import qualified Data.Colour.SRGB as C (sRGB24show)


toJSONText :: A.ToJSON a => a -> T.Text
toJSONText e = T.decodeUtf8 $ LBS.toStrict $ A.encode e

-- | The current schema version is 3
schema :: Int -> String
schema vn = mconcat ["https://vega.github.io/schema/vega-lite/v", show vn,".json"]

-- | Specification of a vega-lite plot
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
    DataJSON [a]  -- ^ Data rows
  | DataURI String -- ^ Data from URI
  deriving (Eq, Show, Generic)
instance A.ToJSON a => A.ToJSON (Data a) where
  toJSON = \case
    DataJSON vs -> A.object ["values" .= vs]
    DataURI u   -> A.object ["url" .= u]

-- | Layer metadata
data LayerMetadata = LayerMD Mark EncSet deriving (Eq, Show, Generic)
instance A.ToJSON LayerMetadata where
  toJSON (LayerMD m e) = A.object ["mark" .= m, "encoding" .= e]

newtype EncSet  = EncSet (S.Set Encoding) deriving (Eq, Show, Generic)
instance A.ToJSON EncSet where
  toJSON (EncSet es) = A.object $ S.foldr insf [] es where
    insf el acc = case el of 
      EsPosX emd   -> ("x" .= emd) : acc
      EsPosY emd   -> ("y" .= emd) : acc
      EsPosX2 emd  -> ("x2" .= emd) : acc
      EsPosY2 emd  -> ("y2" .= emd) : acc
      EsColour emd -> ("color" .= emd) : acc
      EsSize emd   -> ("size" .= emd) : acc      

singleton :: Encoding -> EncSet
singleton = EncSet . S.singleton

instance Semigroup EncSet where
  (EncSet s1) <> (EncSet s2) = EncSet $ s1 <> s2

posXEnc :: T.Text -> EncodingType -> EncSet
posXEnc f t = singleton $ EsPosX $ EncMD f t

posYEnc :: T.Text -> EncodingType -> EncSet
posYEnc f t = singleton $ EsPosY $ EncMD f t

colourFixed :: C.Colour Double -> EncSet
colourFixed c = singleton $ EsColour $ ColourFixed c

colourEnc :: T.Text -> EncodingType -> EncSet
colourEnc f t = singleton $ EsColour $ ColourEnc $ EncMD f t

sizeEnc :: T.Text -> EncodingType -> EncSet
sizeEnc f t = singleton $ EsSize $ SizeEnc $ EncMD f t

-- | Encoding channels for a layer
data Encoding =
    EsPosX EncMetadata
  | EsPosY EncMetadata
  | EsPosX2 EncMetadata
  | EsPosY2 EncMetadata  
  | EsColour Colour
  | EsSize Size
  deriving (Eq, Show, Ord, Generic)
instance A.ToJSON Encoding where
  toJSON = \case
    EsPosX emd   -> A.object ["x" .= emd]
    EsPosY emd   -> A.object ["y" .= emd]
    EsPosX2 emd  -> A.object ["x2" .= emd]
    EsPosY2 emd  -> A.object ["y2" .= emd]
    EsColour emd -> A.object ["color" .= emd]
    EsSize emd   -> A.object ["size" .= emd]  

newtype Mark = Mark { mType :: MarkType } deriving (Eq, Show, Generic)
instance A.ToJSON Mark where
  toJSON (Mark mty) = A.object ["type" .= mty]

-- | Mark types
data MarkType = MPoint | MCircle | MRect | MBar | MArea | MRule deriving (Eq, Show, Generic)
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
