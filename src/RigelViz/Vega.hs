{-# language DeriveGeneric, DeriveDataTypeable, LambdaCase, OverloadedStrings, CPP #-}
module RigelViz.Vega where

-- import qualified Data.Set as S
import GHC.Generics (Generic(..))
import qualified Data.Aeson as A
import Data.Aeson ((.=))
-- import Data.Char (toLower)
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

import Data.Typeable (Typeable)
import Control.Exception (Exception(..))
import Control.Monad.Catch (MonadThrow(..))

import Prelude hiding (lookup)

#if !MIN_VERSION_base(4,8,0)
import Data.Semigroup
#endif


{- |
A plot can be seen as a mapping between data features and features of visual marks. For example:

  * line chart : data.x -> X, data.y -> Y
  * heatmap    : data.x -> X, data.y -> Y, data.z -> COLOUR
-}


-- * Exceptions

data VegaE =
    DataNotFoundE String
  | ScaleNotFoundE String
  deriving (Eq, Typeable)
instance Show VegaE where
  show = \case
    DataNotFoundE dn -> unwords ["dataset <", dn, "> not found"]
    ScaleNotFoundE dn -> unwords ["scale <", dn, "> not found"]    
instance Exception VegaE






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
  , vsScales :: Scales  -- ^ scales
  , vsAxes :: [Axis]  -- ^ axes
  , vsData :: Data r -- ^ data
  , vsMarks :: Marks -- ^ marks
  } deriving  (Eq, Show, Generic)
instance A.ToJSON r => A.ToJSON (VSpec r) where
  toJSON (VSpec tm lm w h p scs axs ds mks) =
    A.object $ [
        "width" .= w
      , "height" .= h
      , "padding" .= p
      , "$schema" .= schema 5
      , "scales" .= scs
      , "axes" .= axs
      , "data" .= ds
      , "marks" .= mks
      ] ++ opts where
    opts = maybeSection "title" tm ++
           maybeSection "legend" lm
           
vegaSpec
  :: Maybe Title
  -> Maybe Legend
  -> Int
  -> Int
  -> Int
  -> Scales
  -> [Axis]
  -> Data r
  -> Marks
  -> VSpec r
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

newtype Data r = Data {
  unData :: M.Map String (DataSource r)
                   } deriving (Eq, Show, Generic)
instance A.ToJSON a => A.ToJSON (Data a) where
  toJSON scs = array $ M.foldlWithKey insf [] $ unData scs where
    insf acc dname ds = case ds of
      DsURI durl -> wrap ["name" .= dname, "url" .= durl] ++ acc
      DsJSON dvs -> wrap ["name" .= dname, "values" .= dvs] ++ acc

data DataSource r =
    DsURI String
  | DsJSON [r]
  deriving (Eq, Show, Generic)

lookupData :: String -> Data a -> Maybe (DataSource a)
lookupData dn (Data dnm) = M.lookup dn dnm







-- * Scale

-- | Scales
-- 
-- https://vega.github.io/vega/docs/scales/
-- | A set of scales is a map from names to 'Scale' metadata
newtype Scales = Scales { unScales :: M.Map String Scale } deriving (Eq, Show, Generic)

instance Semigroup Scales where
  (Scales scs1) <> (Scales scs2) = Scales $ scs1 <> scs2

lookupScale :: String -> Scales -> Maybe Scale
lookupScale scn (Scales sm) = M.lookup scn sm

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


-- crate a 'Scale' only if the referred 'Data'set exists
mkScale :: Data a -> String -> String -> ScaleType -> Maybe Scale
mkScale dat dname dfield stype = do
  _ <- lookupData dname dat
  let dom = Domain dname dfield
  pure $ Scale stype dom
  


data Scale = Scale { scaleType :: ScaleType, scaleDomain :: Domain } deriving (Eq, Show, Generic)

data ScaleType =
    STLinear PlotRange Bool  -- ^ data : linear
  | STBand { stBandPadding :: Double, stBandRange :: PlotRange }  -- ^ data : band
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

encodeDomain dats dn df =
  maybeThrow (DataNotFoundE dn) (lookupData dn dats) $ \ _ ->
    pure $ Domain dn df





-- * Axis

data Axis =
    XAxis XAxisType AxisMetadata
  | YAxis YAxisType AxisMetadata
  deriving (Eq, Show, Generic)
instance A.ToJSON Axis where
  toJSON = \case
    XAxis o amd -> A.object $ ("orient" .= o) : axisMDPairs amd
    YAxis o amd -> A.object $ ("orient" .= o) : axisMDPairs amd

axisMDPairs :: A.KeyValue a => AxisMetadata -> [a]
axisMDPairs (AxisMD s t off tms gr) = ["scale" .= s, "title" .= t, "offset" .= off, "tickMinStep" .= tms, "grid" .= gr]

data AxisMetadata = AxisMD { axScale :: String, axTitle :: String, axOffset :: Int, axTixkMinStep :: Int, axGrid :: Bool} deriving (Eq, Show, Generic)

-- create axis metadata only if the referred 'Scale' exists
mkAxisMD :: Scales -> String -> String -> Int -> Int -> Bool -> Maybe AxisMetadata
mkAxisMD scs scname axt axoff axtms axgr = do
  _ <- lookupScale scname scs
  pure $ AxisMD scname axt axoff axtms axgr



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

newtype Marks = Marks [Mark] deriving (Eq, Show, Generic)
instance A.ToJSON Marks where
  toJSON (Marks mks) = A.object ["marks" .= map A.toJSON mks]

-- | Mark geometry encoding
data Mark =
    MRectC String MGeomEnc MGeomEnc MGeomEnc MGeomEnc -- ^ from.data, xc, yc, w, h
  | MRectV String MGeomEnc MGeomEnc MGeomEnc MGeomEnc -- ^ from.data, x, y, width, y2
  | MSymbol String MarkSymbolShape MGeomEnc MGeomEnc MGeomEnc -- ^ from.data, shape, x, y, size
  | MGroup [Mark] -- ^ "group"
  deriving (Eq, Show, Generic)

instance A.ToJSON Mark where
  toJSON = \case 
    MGroup mges  -> A.object [
      "type" .= string "group",
      "marks" .= map A.toJSON mges
                             ]
    MRectC dfrom xc yc w h -> A.object [
        "type" .= string "rect"
      , "from" .= A.object ["data" .= dfrom]
      , "encode" .= A.object [
          "enter" .=
            A.object [
                "xc" .= xc
              , "yc" .= yc
              , "width" .= w
              , "height" .= h 
              ]
          ]
      ]
    MRectV dfrom x y w y2 -> A.object [
        "type" .= string "rect"
      , "from" .= A.object ["data" .= dfrom]
      , "encode" .= A.object [
          "enter" .=
            A.object [
                "x" .= x
              , "y" .= y
              , "width" .= w
              , "y2" .= y2
              ]
          ]
      ]
    MSymbol dfrom msh x y sz -> A.object [
        "type" .= string "symbol"
      , "from" .= A.object ["data" .= dfrom]
      , "encode" .= A.object [
          "enter" .=
            A.object [
                "shape" .= msh
              , "x" .= x
              , "y" .= y
              , "size" .= sz
              ]
          ]
      ]






-- -- ** Mark color metadata

-- newtype MarkCol = MarkCol [MarkColType] deriving (Eq, Show, Generic)
-- instance Semigroup MarkCol where
--   (MarkCol mc1) <> (MarkCol mc2) = MarkCol $ mc1 <> mc2

-- fill, stroke :: MarkColEnc -> MarkColAlphaEnc -> MarkCol
-- fill ce ae = MarkCol [MCTFill ce ae]
-- stroke ce ae = MarkCol [MCTStroke ce ae]

-- data MarkColType =
--     MCTFill MarkColEnc MarkColAlphaEnc  -- ^ fill
--   | MCTStroke MarkColEnc MarkColAlphaEnc  -- ^ stroke
--   deriving (Eq, Show, Generic)

-- -- encodeMarkColTYpe = \case
-- --   MCTFill mce mcae -> case mce of
-- --     MCECol c   -> ["fill" .= string (T.pack $ C.sRGB24show c)]
-- --       -- MCEEnc emd -> []

-- data MarkColAlphaEnc =
--     MCAEAlpha Double         -- ^ constant alpha
--   | MCAEEnc EncodingMetadata -- ^ alpha encoding channel
--   deriving (Eq, Show, Generic)

-- constAlpha :: Double -> MarkColAlphaEnc
-- constAlpha = MCAEAlpha

-- encAlpha :: String -> String -> MarkColAlphaEnc
-- encAlpha es ef = MCAEEnc $ EncMD es ef

-- data MarkColEnc =
--     MCECol (C.Colour Double)  -- ^ constant colour
--   | MCEEnc EncodingMetadata   -- ^ colour encoding channel
--   deriving (Eq, Show, Generic)

-- constCol :: C.Colour Double -> MarkColEnc
-- constCol = MCECol

-- encCol :: String -> String -> MarkColEnc
-- encCol es ef = MCEEnc $ EncMD es ef






-- | mark geometry encoding : either a value or an encoding channel (scale + field)
data MGeomEnc =
    MGEValueFloat Double
  | MGEEncMD EncodingMetadata
  deriving (Eq, Show, Generic)
instance A.ToJSON MGeomEnc where
  toJSON = \case
    MGEValueFloat x -> A.object ["value" .= x]
    MGEEncMD emd    -> A.toJSON emd

constG = MGEValueFloat

-- build 'EncodingMetadata' only if the referred 'Scale' exists
mkEncMD :: Scales -> String -> String -> Maybe EncodingMetadata
mkEncMD scs sname emf = do
  _ <- lookupScale sname scs
  pure $ EncMD sname emf

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





-- * 'exceptions' utils

maybeThrow :: (MonadThrow m, Exception e) =>
              e -> Maybe a -> (a -> m b) -> m b
maybeThrow e mx f = maybe (throwM e) f mx

-- * 'aeson' utils

wrap :: [(T.Text, A.Value)] -> [A.Value]
wrap xs = [ A.object xs ]  
        
string :: T.Text -> A.Value
string = A.String

array :: [A.Value] -> A.Value
array = A.Array . V.fromList
