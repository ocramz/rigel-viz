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
  , vsWidth :: Int -- ^ plot width [px]
  , vsHeight :: Int  -- ^ plot height [px]
  , vsPadding :: Int  -- ^ padding [px]
  , vsData :: Data a -- ^ data
  } deriving  (Eq, Show, Generic)
instance A.ToJSON a => A.ToJSON (VSpec a) where
  toJSON (VSpec tm w h p ds) =
    A.object $ ["width" .= w, "height" .= h, "padding" .= p, "$schema" .= schema 5, "data" .= ds] ++ opts where
    opts = case tm of
      Just t -> ["title" .= t]
      Nothing -> []
    




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



-- * Data

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

data ScaleType = STLinear | STTime | STBand deriving (Eq, Show, Generic)
instance A.ToJSON ScaleType where
  toJSON = \case
    STLinear -> "linear"
    STTime -> "time"
    STBand -> "band"


data EncodingMetadata s = EncMD { emdScale :: s, emdField :: String } deriving (Eq, Show, Generic)



-- * Mark

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
