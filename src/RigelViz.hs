{-# language OverloadedStrings, DeriveGeneric, LambdaCase #-}
module RigelViz where

import GHC.Generics (Generic(..))

import Lucid
import Lucid.PreEscaped (scriptSrc)
import Lucid.VegaLite (mkVegaHtml)

import qualified Data.Aeson as A
import Data.Aeson ((.=))
import qualified Data.Aeson.Encode.Pretty as A (encodePretty)

import qualified Data.Text as T
import qualified Data.Text.Encoding as T (decodeUtf8)

-- import qualified Data.ByteString as BS hiding (pack)
import qualified Data.ByteString.Lazy.Char8 as BS (unpack)
-- import qualified Data.ByteString.Lazy as LBS

import Data.Monoid

data TestValue = TV { tva :: Int, tvb :: Double } deriving (Eq, Show, Generic)
instance A.ToJSON TestValue
testVs = [TV 1 3.2, TV 2 5.4]


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


data Data a =
    DataJSON [a]
  | DataURL String deriving (Eq, Show, Generic)
instance A.ToJSON a => A.ToJSON (Data a) where
  toJSON = \case
    DataJSON vs -> A.object ["values" .= vs]
    DataURL u   -> A.object ["url" .= u]

data View =
    VSingle Mark Encoding
  | VLayer [View]
  deriving (Eq, Show, Generic)
instance A.ToJSON View 

data Mark = MPoint | MRect | MBar deriving (Eq, Show, Generic)
instance A.ToJSON Mark where
  toJSON = \case
    MPoint -> "point"
    MRect -> "rect"
    MBar -> "bar"

data Encoding = Enc { encName :: T.Text, encMd :: EncMetadata } deriving (Eq, Show, Generic)
instance A.ToJSON Encoding where
  toJSON (Enc en emd) = A.object [en .= emd]

data EncMetadata = EncMetadata { emField :: String, emType :: EncodingType } deriving (Eq, Show, Generic)
instance A.ToJSON EncMetadata where
  toJSON (EncMetadata f t) = A.object [ "field" .= f, "type" .= t]

data EncodingType = ETNominal | ETQuantitative deriving (Eq, Show, Generic)
instance A.ToJSON EncodingType where
  toJSON = \case
    ETNominal -> "nominal"
    ETQuantitative -> "quantitative"

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
