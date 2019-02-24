{-# language OverloadedStrings, DeriveGeneric, LambdaCase #-}
module RigelViz where

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
  | DataURL String -- ^ Data from URI
  deriving (Eq, Show, Generic)
instance A.ToJSON a => A.ToJSON (Data a) where
  toJSON = \case
    DataJSON vs -> A.object ["values" .= vs]
    DataURL u   -> A.object ["url" .= u]

data LayerMetadata = LayerMD Mark Encoding deriving (Eq, Show, Generic)
instance A.ToJSON LayerMetadata where
  toJSON (LayerMD m e) = A.object ["mark" .= m, "encoding" .= e]

newtype Colour = Colour (C.Colour Double) deriving (Eq, Show, Generic)
instance A.ToJSON Colour where
  toJSON (Colour c) = A.String $ T.pack $ C.sRGB24show c
  
data Mark = Mark { mType :: MarkType, mColor :: Colour } deriving (Eq, Show, Generic)
instance A.ToJSON Mark where
  toJSON (Mark mty mcol) = A.object ["type" .= mty, "color" .= mcol]

data MarkType = MPoint | MRect | MBar | MArea | MRule deriving (Eq, Show, Generic)
instance A.ToJSON MarkType where
  toJSON = \case
    MPoint -> "point"
    MRect  -> "rect"
    MBar   -> "bar"
    MArea  -> "area"
    MRule  -> "rule"

data Encoding = Enc { encsX :: EncMetadata, encsY ::  EncMetadata, encsColor :: Maybe EncMetadata, encsX2 :: Maybe EncMetadata, encsY2 :: Maybe EncMetadata } deriving (Eq, Show, Generic)
instance A.ToJSON Encoding where
  toJSON (Enc ex ey ec ex2 ey2) = A.object $
      encMaybeKV "color" ec ++
      encMaybeKV "x2" ex2 ++
      encMaybeKV "y2" ey2 ++
      def
        where
          def = ["x" .= ex, "y" .= ey]

encMaybeKV :: (A.KeyValue a, A.ToJSON v) => T.Text -> Maybe v -> [a]
encMaybeKV k = maybe [] (\c -> [k .= c])

data EncMetadata = EncMD { encField :: T.Text, emType :: EncodingType } deriving (Eq, Show, Generic)
instance A.ToJSON EncMetadata where
  toJSON (EncMD f t) = A.object [ "field" .= f, "type" .= t]

data EncodingType = ETNominal | ETQuantitative | ETTemporal | ETOrdinal deriving (Eq, Show, Generic)
instance A.ToJSON EncodingType where
  toJSON = \case
    ETNominal      -> "nominal"
    ETQuantitative -> "quantitative"
    ETTemporal     -> "temporal"
    ETOrdinal      -> "ordinal"

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
