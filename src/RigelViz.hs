{-# language OverloadedStrings, DeriveGeneric, LambdaCase #-}
{-# language CPP #-}
{-|
Module      : RigelViz
Description : Mid-level bindings to @vega-lite@
Copyright   : (c) Marco Zocca, 2019
License     : BSD3
Maintainer  : ocramz fripost org
Stability   : experimental
Portability : POSIX


A (mid-level, simplified, opinionated) Haskell wrapper for [vega-lite](https://vega.github.io/vega-lite/), currently targeting version 3 of the @vega-lite@ schema.

== Aims / definitions

* mid-level :

    * types which can take one of a few possible values are represented by sum types, not by strings.

    * glyph colours are encoded via the @colour@ Haskell library.

* simplified : the generated @vega-lite@ JSON is not normalized, i.e. has some redundancies. This reflects the internal representation but also makes it easier to reason "locally" (i.e. code sections don't visibly exploit inheritance from higher layers).

* opinionated : part of the @vega-lite@ API is not used at all. For example, there is no support for data preprocessing (e.g. summarization etc.). This forces the user to use the host language for preprocessing, which is bound to be more expressive and robust.

== Examples

These examples require @lucid@ and @lucid-extras@ (> 0.2.2): @lucid@ provides @renderToFile@ and @lucid-extras@ provides @mkVegaHtml@.

=== Scatter plot

<<doc/fig/scatter1.png>>

@
render0 :: IO ()
render0 = 'renderToFile' "scatter.html" $ 'mkVegaHtml' $ 'A.toJSON' vls0

vls0 :: 'VLSpec' TestValue
vls0 =
  'vegaLiteSpec' 400 300 [
    'layer' 'MCircle' ('DataJSON' testVs) (
       'posEnc' 'X' "tv" 'Nominal' <>
       posEnc 'Y' "tvb" 'Quantitative' <>
       'colourEnc' "tvb" Quantitative <>
       'sizeEnc' "tvb" Quantitative
       )
    ]

data TestValue = TV { tv :: T, tvb :: Double } deriving (Eq, Show, Generic)
instance A.ToJSON TestValue
data T = A | B | C deriving (Eq, Show, Generic)
instance A.ToJSON T

testVs :: [TestValue]
testVs = [TV A 3.2, TV B 5.4, TV A 2.2, TV A 6.7, TV B 4.9]
@


=== Heatmap

<<doc/fig/heatmap1.png>>

@
render0 :: IO ()
render0 = 'renderToFile' "heatmap.html" $ 'mkVegaHtml' $ 'A.toJSON' vls1

vls1 :: VLSpec (V3 Double)
vls1 = vegaLiteSpec 400 400 [
  layer 'MRect' (DataJSON dats) $
      posEnc X "v3x" 'Ordinal' <>
      posEnc Y "v3y" Ordinal  <>
      colourEnc "v3z" Quantitative 

data V3 a = V3 { v3x :: a, v3y :: a, v3z :: a } deriving (Eq, Show, Generic)
instance A.ToJSON a => A.ToJSON (V3 a)

dats :: [V3 Double]
dats = [V3 x y (f x y) | x <- xs, y <- ys] where
  xs = map (/10) [0, 1 .. 20]
  ys = xs
  f x y = sin $ 2 * pi * sqrt (x ** 2 + y ** 2)
@



-}
module RigelViz (
  vegaLiteSpec, VLSpec,
  -- * Data sources
  DataSource(..),
  -- * Layer
  layer,  LayerMetadata,
  -- * Mark 
  MarkType(..),
  -- * Data encoding options
  EncSet, posEnc, Pos(..), colourEnc, colour, size, sizeEnc, EncodingType(..), bounds, range) where

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

#if !MIN_VERSION_base(4,8,0)
import Data.Semigroup
#endif


-- toJSONText :: A.ToJSON a => a -> T.Text
-- toJSONText e = T.decodeUtf8 $ LBS.toStrict $ A.encode e

-- | The current schema version is 3
schema :: Int -> String
schema vn = mconcat ["https://vega.github.io/schema/vega-lite/v", show vn,".json"]

-- | Create a @vega-lite@ spec
vegaLiteSpec ::
     Int  -- ^ Plot width
  -> Int  -- ^ Plot height
  -> [LayerMetadata a d]  
  -> VLSpec a d
vegaLiteSpec = VLSpec

-- | Specification of a vega-lite plot
--
-- A 'VLSpec' can be encoded into a JSON blob via its 'A.ToJSON' instance.
data VLSpec a d = VLSpec {
    vlsWidth :: Int
  , vlsHeight :: Int
  , vlsView :: [LayerMetadata a d]
  } deriving (Eq, Show, Generic)
instance (A.ToJSON a, A.ToJSON d) => A.ToJSON (VLSpec a d) where
  toJSON (VLSpec w h lms) = A.object $ ("layer" .= map A.toJSON lms) : defs
    where
      defs = [
        "$schema" .= schema 3
        , "width" .= w
        , "height" .= h
        ]

-- | Data source
data DataSource a =
    DataJSON [a]  -- ^ Data row type must have a 'A.ToJSON' instance
  | DataURI String -- ^ URI or filepath of dataset
  deriving (Eq, Show, Generic)
instance A.ToJSON a => A.ToJSON (DataSource a) where
  toJSON = \case
    DataJSON vs -> A.object ["values" .= vs]
    DataURI u   -> A.object ["url" .= u]

-- | Plot layer data and encoding metadata
data LayerMetadata a d = LayerMD Mark (DataSource d) (EncSet a) deriving (Eq, Show, Generic)
instance (A.ToJSON a, A.ToJSON d) => A.ToJSON (LayerMetadata a d) where
  toJSON (LayerMD m ds e) = A.object ["mark" .= m, "encoding" .= e, "data" .= ds]

-- | Declare a plot layer
layer :: MarkType -> DataSource d -> EncSet a -> LayerMetadata a d
layer m ds es = LayerMD (Mark m True) ds es

-- | Set of channel encoding options.
--
-- Options are created with 'posEnc', 'colourEnc', 'colour', 'sizeEnc', 'size' and can be added to an 'EncSet' via its 'Semigroup' instance
newtype EncSet a = EncSet (S.Set (Encoding a)) deriving (Eq, Show, Generic)
instance A.ToJSON a => A.ToJSON (EncSet a) where
  toJSON (EncSet es) = A.object $ S.foldr insf [] es where
    insf el acc = case el of
      EsPos p emd  -> (showPos p .= emd) : acc
      EsColour emd -> ("color" .= emd) : acc
      EsSize emd   -> ("size" .= emd) : acc      

singleton :: Encoding a -> EncSet a
singleton = EncSet . S.singleton

instance Ord a => Semigroup (EncSet a) where
  (EncSet s1) <> (EncSet s2) = EncSet $ s1 <> s2

-- | Position encoding
posEnc :: Pos
  -> T.Text   -- ^ Field in the data source
  -> EncodingType 
  -> Domain a
  -> EncSet a
posEnc p f t d = singleton $ EsPos p $ EncMD f t d

-- | Fixed colour
colour :: C.Colour Double -> EncSet a
colour c = singleton $ EsColour $ ColourFixed c

-- | Colour encoding
colourEnc :: T.Text -- ^ Field in the data source
  -> EncodingType
  -> Domain a
  -> EncSet a
colourEnc f t d = singleton $ EsColour $ ColourEnc $ EncMD f t d

-- | Size encoding
sizeEnc :: T.Text -- ^ Field in the data source
  -> EncodingType
  -> Domain a
  -> EncSet a
sizeEnc f t d = singleton $ EsSize $ SizeEnc $ EncMD f t d

-- | Fixed size
size :: Double -> EncSet a
size s = singleton $ EsSize $ SizeFixed s

-- | Encoding channels for a layer
data Encoding a =
    EsPos Pos (EncMetadata a)
  | EsColour (Colour a)
  | EsSize (Size a)
  deriving (Eq, Show, Ord, Generic)

-- | Position encoding alternatives
data Pos = X | Y | X2 | Y2 deriving (Eq, Ord, Show)
showPos :: Pos -> T.Text
showPos p = T.pack $ map toLower $ show p

data Mark = Mark { mType :: MarkType, mClip :: Bool } deriving (Eq, Show, Generic)
instance A.ToJSON Mark where
  toJSON (Mark mty mcl) = A.object ["type" .= mty, "clip" .= mcl]

-- | Mark type alternatives
data MarkType =
    MPoint  -- ^ "point"
  | MCircle -- ^ "circle"
  | MRect   -- ^ "rect"
  | MSquare -- ^ "square"   
  | MBar    -- ^ "bar"
  | MArea   -- ^ "area"
  | MRule   -- ^ "rule"
  | MLine   -- ^ "line"  
  deriving (Eq, Show, Generic)
instance A.ToJSON MarkType where
  toJSON = \case
    MPoint  -> "point"
    MCircle -> "circle"
    MRect   -> "rect"
    MSquare -> "square"    
    MBar    -> "bar"
    MArea   -> "area"
    MRule   -> "rule"
    MLine   -> "line"

data EncMetadata a = EncMD { encField :: T.Text, emType :: EncodingType, emScale :: Domain a } deriving (Eq, Show, Ord, Generic)
instance A.ToJSON a => A.ToJSON (EncMetadata a) where
  toJSON (EncMD f t d) = A.object [ "field" .= f, "type" .= t, "scale" .= d]

-- | Scale bounds (used in Quantitative and Temporal channels)
bounds :: a -> a -> Domain a
bounds mi ma = Domain [mi, ma]

-- | Scale elements (used in Ordinal and Nominal channels)
range :: [a] -> Domain a
range = Domain

newtype Domain a = Domain [a] deriving (Eq, Show, Ord)
instance A.ToJSON a => A.ToJSON (Domain a) where
  toJSON (Domain ds) = A.object ["domain" .= A.toJSON ds]

-- | "colour" encoding channel metadata
data Colour a =
    ColourFixed (C.Colour Double)
  | ColourEnc (EncMetadata a)
  deriving (Eq, Show, Generic)
instance Ord a => Ord (Colour a) where
  ColourFixed c1 <= ColourFixed c2 = C.sRGB24show c1 <= C.sRGB24show c2 -- ewwwwwww
  ColourEnc e1 <= ColourEnc e2 = e1 <= e2
  _ <= _ = False
instance A.ToJSON a => A.ToJSON (Colour a) where
  toJSON = \case
    ColourFixed c -> A.object ["value" .= C.sRGB24show c]
    ColourEnc emd -> A.toJSON emd

-- | "size" encoding channel metadata
data Size a =
    SizeFixed Double
  | SizeEnc (EncMetadata a)
  deriving (Eq, Show, Ord, Generic)
instance A.ToJSON a => A.ToJSON (Size a) where
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
