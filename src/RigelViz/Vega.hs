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
    vsWidth :: Int
  , vsHeight :: Int
  , vsPadding :: Int
  } deriving  (Eq, Show, Generic)
instance A.ToJSON VSpec where
  toJSON (VSpec w h p) =
    A.object ["width" .= w, "height" .= h, "padding" .= p, "$schema" .= schema 5]




-- | Create a title with some default settings
titleDef :: String -> Title
titleDef t = Title t TMiddle 15 TFGroup 5

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
