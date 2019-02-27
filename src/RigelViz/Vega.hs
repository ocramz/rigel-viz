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

#if !MIN_VERSION_base(4,8,0)
import Data.Semigroup
#endif


-- | The current schema version is 5
schema :: Int -> String
schema vn = mconcat ["https://vega.github.io/schema/vega/v", show vn,".json"]


-- | Specification of a vega plot
--
-- A 'VSpec' can be encoded into a JSON blob via its 'A.ToJSON' instance.
data VSpec = VSpec {
    vsWidth :: Int -- ^ plot width [px]
  , vsHeight :: Int  -- ^ plot height [px]
  , vsPadding :: Int  -- ^ padding [px]
  } deriving  (Eq, Show, Generic)
instance A.ToJSON VSpec where
  toJSON (VSpec w h p) =
    A.object ["width" .= w, "height" .= h, "padding" .= p, "$schema" .= schema 5]




-- | Create a title with some default settings
titleDef ::
     String   -- ^ title
  -> Int  -- ^ font size
  -> Title
titleDef t s = Title t TMiddle s TFGroup 5

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
  toJSON (Data dn ds) = let nn = ["name" .= dn] in case ds of
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




-- * Mark

-- | Mark type alternatives
data MarkType =
    MCircle -- ^ "circle"
  | MRect   -- ^ "rect"
  | MArea   -- ^ "area"
  | MRule   -- ^ "rule"
  | MLine   -- ^ "line"  
  deriving (Eq, Show, Generic)
instance A.ToJSON MarkType where
  toJSON = \case
    MCircle -> "circle"
    MRect   -> "rect"
    MArea   -> "area"
    MRule   -> "rule"
    MLine   -> "line"
