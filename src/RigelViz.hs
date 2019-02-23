{-# language OverloadedStrings, DeriveGeneric, LambdaCase #-}
module RigelViz where

import GHC.Generics (Generic(..))

import qualified Data.Aeson as A
import Data.Aeson ((.=))
import qualified Data.Aeson.Encode.Pretty as A (encodePretty)

import qualified Data.Text as T
import qualified Data.Text.Encoding as T (encodeUtf8, decodeUtf8)

-- import qualified Data.ByteString as BS hiding (pack)
import qualified Data.ByteString.Lazy.Char8 as BS (unpack)
import qualified Data.ByteString.Lazy as LBS (toStrict)

import Data.Monoid


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
  , vlsView :: View
  } deriving (Eq, Show, Generic)
instance A.ToJSON a => A.ToJSON (VLSpec a) where
  toJSON (VLSpec w h dats view) = case view of
    VLayer vs -> A.object $ ("layer" .= map A.toJSON vs) : defs
    VSingle mark enc -> A.object $ [
        "mark" .= mark
      , "encoding" .= enc
      ] ++ defs
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
  | DataURL String -- ^ Data from URI
  deriving (Eq, Show, Generic)
instance A.ToJSON a => A.ToJSON (Data a) where
  toJSON = \case
    DataJSON vs -> A.object ["values" .= vs]
    DataURL u   -> A.object ["url" .= u]

data View =
    VSingle Mark Encodings
  | VLayer [View]
  deriving (Eq, Show, Generic)
instance A.ToJSON View 

data Mark = MPoint | MRect | MBar | MArea deriving (Eq, Show, Generic)
instance A.ToJSON Mark where
  toJSON = \case
    MPoint -> "point"
    MRect  -> "rect"
    MBar   -> "bar"
    MArea  -> "area"

data Encodings = Encs { encsX :: EncMetadata, encsY ::  EncMetadata, encsColor :: Maybe EncMetadata, encsX2 :: Maybe EncMetadata, encsY2 :: Maybe EncMetadata } deriving (Eq, Show, Generic)
instance A.ToJSON Encodings where
  toJSON (Encs ex ey ec ex2 ey2) = A.object ["x" .= ex, "y" .= ey]

-- data Encoding = Enc { encChannel :: EncChannel, encMd :: EncMetadata } deriving (Eq, Show, Generic)
-- instance A.ToJSON Encoding where
--   toJSON (Enc ec emd) = A.object [toJSONText ec .= emd]

-- data EncChannel = X | Y | Color| X2 | Y2 deriving (Eq, Show, Generic)
-- instance A.ToJSON EncChannel where
--   toJSON = \case
--     X -> "x"
--     Y -> "y"
--     Color -> "color"
--     X2 -> "x2"
--     Y2 -> "y2"

data EncMetadata = EncMetadata { encField :: T.Text, emType :: EncodingType } deriving (Eq, Show, Generic)
instance A.ToJSON EncMetadata where
  toJSON (EncMetadata f t) = A.object [ "field" .= f, "type" .= t]

data EncodingType = ETNominal | ETQuantitative | ETTemporal deriving (Eq, Show, Generic)
instance A.ToJSON EncodingType where
  toJSON = \case
    ETNominal -> "nominal"
    ETQuantitative -> "quantitative"
    ETTemporal -> "temporal"

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
