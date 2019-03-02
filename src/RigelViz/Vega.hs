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
-- A 'VSpec r' can be encoded into a JSON blob via its 'A.ToJSON' instance.
--
-- NB : 'r' is the type of the rows

data VSpec r = VSpec {
    vsTitle :: Maybe Title  -- ^ title
  , vsLegend :: Maybe Legend
  , vsWidth :: Int -- ^ plot width [px]
  , vsHeight :: Int  -- ^ plot height [px]
  , vsPadding :: Int  -- ^ padding [px]
  , vsData :: Data r -- ^ data
  } deriving  (Eq, Show, Generic)
instance A.ToJSON r => A.ToJSON (VSpec r) where
  toJSON (VSpec tm lm w h p ds) =
    A.object $ ["width" .= w, "height" .= h, "padding" .= p, "$schema" .= schema 5, "data" .= ds] ++ opts where
    opts = maybeSection "title" tm ++
           maybeSection "legend" lm

vegaSpec = VSpec

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
-- If the format property is not specified, the data is assumed to be in a row-oriented JSON format.

newtype Data a = Data {
  unData :: M.Map String (Either String [a])
                   } deriving (Eq, Show, Generic)
instance A.ToJSON a => A.ToJSON (Data a) where
  toJSON scs = array $ M.foldlWithKey insf [] $ unData scs where
    insf acc dname ee = case ee of
      Left durl -> wrap ["name" .= dname, "url" .= durl] ++ acc
      Right dvs -> wrap ["name" .= dname, "values" .= dvs] ++ acc






-- * Scale

-- | Scales
-- 
-- https://vega.github.io/vega/docs/scales/
-- | A set of scales is a map from names to 'Scale' metadata
newtype Scales = Scales { unScales :: M.Map String Scale } deriving (Eq, Show, Generic)

instance A.ToJSON Scales where
  toJSON scs = array $ M.foldlWithKey insf [] $ unScales scs where
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

wrap xs = [ A.object xs ]  
        
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

data Mark = Mark MarkGE deriving (Eq, Show, Generic)

-- | Mark geometry encoding
data MarkGE =
    MRectC MarkGeomEnc MarkGeomEnc MarkGeomEnc MarkGeomEnc -- ^ xc, yc, w, h
  | MSymbol MarkSymbolShape MarkGeomEnc MarkGeomEnc MarkGeomEnc -- ^ shape, x, y, size
  | MGroup [MarkGE] -- ^ "group"
  deriving (Eq, Show, Generic)
-- instance A.ToJSON Mark where
--   toJSON = \case



-- -- | mark colour encoding : either a colour or an encoding channel (scale + field)
-- data MarkColEnc =
--     MCECol MarkColour
--   | MCEEncMD EncodingMetadata
--   deriving (Eq, Show, Generic)

-- ** Mark color metadata

newtype MarkCol = MarkCol [MarkColType] deriving (Eq, Show, Generic)
instance Semigroup MarkCol where
  (MarkCol mc1) <> (MarkCol mc2) = MarkCol $ mc1 <> mc2

fill, stroke :: MarkColEnc -> MarkColAlphaEnc -> MarkCol
fill ce ae = MarkCol [MCTFill ce ae]
stroke ce ae = MarkCol [MCTStroke ce ae]

data MarkColType =
    MCTFill MarkColEnc MarkColAlphaEnc
  | MCTStroke MarkColEnc MarkColAlphaEnc
  deriving (Eq, Show, Generic)

data MarkColAlphaEnc =
    MCAEAlpha Double
  | MCAEEnc EncodingMetadata
  deriving (Eq, Show, Generic)

constAlpha :: Double -> MarkColAlphaEnc
constAlpha = MCAEAlpha

encAlpha :: String -> String -> MarkColAlphaEnc
encAlpha es ef = MCAEEnc $ EncMD es ef

data MarkColEnc =
    MCECol (C.Colour Double)
  | MCEEnc EncodingMetadata
  deriving (Eq, Show, Generic)

constCol :: C.Colour Double -> MarkColEnc
constCol = MCECol

encCol :: String -> String -> MarkColEnc
encCol es ef = MCEEnc $ EncMD es ef


-- | mark geometry encoding : either a value or an encoding channel (scale + field)
data MarkGeomEnc =
    MGEValueFloat Double
  | MGEEncMD EncodingMetadata
  deriving (Eq, Show, Generic)
instance A.ToJSON MarkGeomEnc where
  toJSON = \case
    MGEValueFloat x -> A.object ["value" .= x]
    MGEEncMD emd    -> A.toJSON emd

-- | Scale metadata to encode one mark feature
--
-- NB : the 'emdScale' field must be the name to an existing 'Scale'
data EncodingMetadata = EncMD {
    emdScale :: String      -- ^ which 'scale' is the data encoded with
  , emdField :: String -- ^ what data field is used
  } deriving (Eq, Show, Ord, Generic)
instance A.ToJSON EncodingMetadata where
  toJSON (EncMD sc scf) = A.object ["scale" .= sc, "field" .= scf]

-- | Shapes for the "symbol" Mark
data MarkSymbolShape =
    MSSCircle  -- ^ "circle"
  | MSSCross   -- ^ "cross"
  deriving (Eq, Show, Generic)
instance A.ToJSON MarkSymbolShape where
  toJSON = \case
    MSSCircle -> "circle"
    MSSCross  -> "cross"





-- ** Mark geometry metadata (position and size)

-- -- | centered (xc, yc, width, height)
-- data MarkGeomCentered a = MarkGeomC { mgcXc :: a, mgcYc :: a , mgcW :: a, mgcH :: a } deriving (Eq, Show, Generic)

-- -- | non centered (x, y, x2, y2)
-- data MarkGeom a = MarkGeom { mgX :: a, mgY :: a , mgX2 :: a, mgY2 :: a } deriving (Eq, Show, Generic)
