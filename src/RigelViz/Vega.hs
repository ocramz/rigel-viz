{-# language DeriveGeneric, LambdaCase, OverloadedStrings, CPP #-}
module RigelViz.Vega where

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
import qualified Data.Map as M
import qualified Data.Vector as V (fromList)

import Prelude hiding (lookup)

#if !MIN_VERSION_base(4,8,0)
import Data.Semigroup
#endif


{- |
A plot can be seen as a mapping between data features and features of visual marks. For example:

  * line chart : data.x -> X, data.y -> Y
  * heatmap    : data.x -> X, data.y -> Y, data.z -> COLOUR
-}








-- | The current schema version is 5
schema :: Int -> String
schema vn = mconcat ["https://vega.github.io/schema/vega/v", show vn,".json"]


-- | Specification of a vega plot
--
-- A 'VSpec a' can be encoded into a JSON blob via its 'A.ToJSON' instance.
--
-- NB : 'a' is the type of the rows
data VSpec a = VSpec {
    vsTitle :: Maybe Title  -- ^ title
  , vsLegend :: Maybe Legend
  , vsWidth :: Int -- ^ plot width [px]
  , vsHeight :: Int  -- ^ plot height [px]
  , vsPadding :: Int  -- ^ padding [px]
  , vsData :: Data a -- ^ data
  } deriving  (Eq, Show, Generic)
instance A.ToJSON a => A.ToJSON (VSpec a) where
  toJSON (VSpec tm lm w h p ds) =
    A.object $ ["width" .= w, "height" .= h, "padding" .= p, "$schema" .= schema 5, "data" .= ds] ++ opts where
    opts = maybeSection "title" tm ++
           maybeSection "legend" lm

maybeSection :: (A.KeyValue a, A.ToJSON v) => T.Text -> Maybe v -> [a]    
maybeSection st = maybe [] (\t -> [st .= t])



-- * Title

-- | Create a title with some default settings
title ::
     String   -- ^ title
  -> Int  -- ^ font size
  -> Title
title t s = Title t TMiddle s TFGroup 5

data Title = Title { titleText :: String, titleAnchor :: TAnchor, titleFontSz :: Int, titleFrame :: TFrame, titleOffset :: Int } deriving (Eq, Show, Generic)
instance A.ToJSON Title where
  toJSON (Title ts ta fs tf off) = A.object ["text" .= ts, "anchor" .= ta, "fontSize" .= fs, "frame" .= tf, "offset" .= off]

data TFrame = TFGroup deriving (Eq, Show, Generic)
instance A.ToJSON TFrame where
  toJSON = \case
    TFGroup -> "group"

-- title anchor
data TAnchor = TStart | TMiddle | TEnd deriving (Eq, Show, Generic)
instance A.ToJSON TAnchor where
  toJSON = \case
    TStart -> "start"
    TMiddle -> "middle"
    TEnd -> "end"


-- * Legend

data Legend = Legend { lType :: LType, lTitle :: String, lTitleFontsize :: Int } deriving (Eq, Show, Generic)
instance A.ToJSON Legend where
  toJSON (Legend lty lt lfs) =
    A.object ["type" .= lty, "title" .= lt, "titleFontSize" .= lfs]

-- | Legend type
data LType = LGradient deriving (Eq, Show, Generic)
instance A.ToJSON LType where
  toJSON = \case
    LGradient -> "gradient"


-- * Data

-- | Each dataset is labeled by a name string
newtype Datasets a = DS (M.Map String [a]) deriving (Eq, Show)

-- | Lookup a dataset by name
lookupDS :: String -> Datasets a -> Maybe [a]
lookupDS n (DS dsm) = M.lookup n dsm

singletonDS :: String -> [a] -> Datasets a
singletonDS n ds = DS $ M.singleton n ds

fromListDS :: [(String, [a])] -> Datasets a
fromListDS = DS . M.fromList 

data Data a = Data { dataName :: String, dataSource :: DataSource a} deriving (Eq, Show, Generic)
instance A.ToJSON a => A.ToJSON (Data a) where
  toJSON (Data dn ds) = let
    nn = ["name" .= dn]
    in case ds of
      DataJSON vs -> A.object $ ("values" .= vs) : nn
      DataURI u -> A.object $ ("url" .= u) : nn 

-- | Data source
-- If the format property is not specified, the data is assumed to be in a row-oriented JSON format.
data DataSource a =
    DataJSON [a]  -- ^ Data row type must have a 'A.ToJSON' instance
  | DataURI String -- ^ URI or filepath of dataset
  deriving (Eq, Show, Generic)
-- instance A.ToJSON a => A.ToJSON (DataSource a) where
--   toJSON = \case
--     DataJSON vs -> A.object ["values" .= vs]
--     DataURI u   -> A.object ["url" .= u]






-- * Scale

-- | Scales
-- 
-- https://vega.github.io/vega/docs/scales/
-- | A set of scales is a map from names to 'Scale' metadata
newtype Scales = Scales { unScales :: M.Map String Scale } deriving (Eq, Show, Generic)

instance A.ToJSON Scales where
  toJSON scs = array $ M.foldlWithKey insf [] $ unScales scs where
    wrap xs = [ A.object xs ]  
    insf acc scName (Scale sty sd) = wrap (["name" .= scName, "domain" .= sd] ++ opts) ++ acc
      where
        opts = case sty of
          STLinear r zb  -> [
            "type"    .= string "linear"
            , "range"   .= r
            , "zero"    .= zb 
            ]
          STBand pad r   -> [
            "type"    .= string "band"
            , "range" .= r
            , "padding" .= pad
            ]
          STColours cols -> case cols of
            ColScheme cs -> [
              "type" .= string "linear"
              , "range" .= A.object ["scheme" .= cs]
              ]
            ColValues cvs -> [
              "type" .= string "ordinal"
              , "range" .= map (string . T.pack . C.sRGB24show) cvs
              ]
        
string :: T.Text -> A.Value
string = A.String

array :: [A.Value] -> A.Value
array = A.Array . V.fromList

data Scale = Scale { scaleType :: ScaleType, scaleDomain :: Domain } deriving (Eq, Show, Generic)

data ScaleType =
    STLinear PlotRange Bool  -- ^ data : linear
  | STBand { stBandPadding :: Double, stBandRange :: PlotRange }    -- ^ data : band
  | STColours Colours  -- ^ colours : linear or band 
  deriving (Eq, Show, Generic)

data PlotRange = Width | Height deriving (Eq, Show, Generic)
instance A.ToJSON PlotRange where
  toJSON = \case
    Width -> "width"
    Height -> "height"

data Colours =
    ColScheme ColourScheme
  | ColValues [C.Colour Double]  -- ^ A list of colours for a discrete palette
  deriving (Eq, Show, Generic)

-- | Colour schemes for e.g. heatmaps
data ColourScheme = CSPlasma deriving (Eq, Show, Generic)
instance A.ToJSON ColourScheme where
  toJSON = \case
    CSPlasma -> "plasma"


data Domain = Domain {domainData :: String, domainField :: String} deriving (Eq, Show, Generic)
instance A.ToJSON Domain where
  toJSON (Domain dd df) = A.object ["data" .= dd, "field" .= df]


-- * Axis

data XAxis = XAxis { xaOrient :: XAxisType, xaMD :: AxisMetadata } deriving (Eq, Show, Generic)
instance A.ToJSON XAxis where
  toJSON (XAxis o amd) = A.object $ ("orient" .= o) : axisMDPairs amd
  
data YAxis s = YAxis { yaOrient :: YAxisType, yaMD :: AxisMetadata } deriving (Eq, Show, Generic)
instance A.ToJSON s => A.ToJSON (YAxis s) where
  toJSON (YAxis o amd) = A.object $ ("orient" .= o) : axisMDPairs amd

axisMDPairs :: A.KeyValue a => AxisMetadata -> [a]
axisMDPairs (AxisMD s t off tms gr) = ["scale" .= s, "title" .= t, "offset" .= off, "tickMinStep" .= tms, "grid" .= gr]

data AxisMetadata = AxisMD { axScale :: String, axTitle :: String, axOffset :: Int, axTixkMinStep :: Int, axGrid :: Bool} deriving (Eq, Show, Generic)



-- | V. axis types
data XAxisType = HATop  | HABottom deriving (Eq, Show, Generic)
instance A.ToJSON XAxisType where
  toJSON = \case
    HATop -> "top"
    HABottom -> "bottom"
data YAxisType = VALeft | VARight deriving (Eq, Show, Generic)
instance A.ToJSON YAxisType where
  toJSON = \case
    VALeft -> "left"
    VARight -> "right"





-- * Mark

-- | Scale metadata to encode one mark feature
data EncodingMetadata s = EncMD {
    emdScale :: s      -- ^ which 'scale' is the data encoded with
  , emdField :: String -- ^ what data field is used
  } deriving (Eq, Show, Generic)

-- | Mark type alternatives
data MarkType =
    MCircle -- ^ "circle"
  | MRect   -- ^ "rect"
  | MArea   -- ^ "area"
  | MRule   -- ^ "rule"
  | MLine   -- ^ "line"
  | MGroup  -- ^ "group"
  deriving (Eq, Show, Generic)
instance A.ToJSON MarkType where
  toJSON = \case
    MCircle -> "circle"
    MRect   -> "rect"
    MArea   -> "area"
    MRule   -> "rule"
    MLine   -> "line"
    MGroup  -> "group"

-- ** Mark color metadata

-- | A shape can be coloured in three ways : fill only, stroke (border) only, both fill and stroke
data MarkColour = MCFill Colour | MCStroke Colour | MCBoth Colour Colour deriving (Eq, Show, Generic)

data Colour = Colour { cFill :: C.Colour Double, cAlpha :: Double } deriving (Eq, Show, Generic)


-- ** Mark geometry metadata (position and size)

-- | centered (xc, yc, width, height)
data MarkGeomCentered a = MarkGeomC { mgcXc :: a, mgcYc :: a , mgcW :: a, mgcH :: a } deriving (Eq, Show, Generic)

-- | non centered (x, y, x2, y2)
data MarkGeom a = MarkGeom { mgX :: a, mgY :: a , mgX2 :: a, mgY2 :: a } deriving (Eq, Show, Generic)
