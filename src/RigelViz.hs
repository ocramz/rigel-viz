{-# language OverloadedStrings, DeriveGeneric #-}
module RigelViz where

import GHC.Generics (Generic(..))

import Lucid
import Lucid.PreEscaped (scriptSrc)
import Lucid.VegaLite (mkVegaHtml)

import qualified Data.Aeson as A

import qualified Data.Text as T
import qualified Data.Text.Encoding as T (decodeUtf8)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

import Data.Monoid

schema :: BS.ByteString
schema = "https://vega.github.io/schema/vega-lite/v3.json"

-- data Data
-- data Mark
-- data Encoding

data VLSpec = VLSpec Data Mark View

data Data = DataJSON | DataURL deriving (Eq, Show, Generic)

data Mark = MarkPoint | MarkBar deriving (Eq, Show, Generic)

data View = ViewSingle | ViewLayer deriving (Eq, Show, Generic)

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
