{-# language DeriveGeneric, DeriveDataTypeable, LambdaCase, OverloadedStrings#-}
----------------------------------------------------------------
-- |
-- Module      :  RigelViz.Vega.Types
-- Description :  Types and instances
-- Copyright   :  (c) Marco Zocca (2019)
-- License     :  MIT
-- Maintainer  :  ocramz fripost org
-- Stability   :  experimental
-- Portability :  GHC
--
-- Types and instances for the library. The 'aeson' instances are meant to mirror
-- the JSON encoding expected by 'vega'
-- 
-----------------------------------------------------------------
{-# OPTIONS_GHC -Wno-unused-imports #-}
module RigelViz.Vega.Types where

import Data.Typeable (Typeable)
import GHC.Generics (Generic(..))
import Control.Exception (Exception(..))
-- aeson
import qualified Data.Aeson as A
import Data.Aeson ((.=))
-- containers
import qualified Data.Map as M
-- exceptions
import Control.Monad.Catch (MonadThrow(..))
-- text
import qualified Data.Text as T
-- vector
import qualified Data.Vector as V (fromList)

-- | The current schema version is 5
schema :: Int -> String
schema vn = mconcat ["https://vega.github.io/schema/vega/v", show vn,".json"]

-- | Specification of a vega plot where 'r' is the type of the data rows
data VSpec r = VSpec {
    vsTitle :: Maybe Title  -- ^ title
  , vsLegend :: Maybe Legend
  , vsWidth :: Int -- ^ plot width [px]
  , vsHeight :: Int  -- ^ plot height [px]
  , vsPadding :: Int  -- ^ padding [px]
  -- , vsScales :: Scales  -- ^ scales
  -- , vsAxes :: [Axis]  -- ^ axes
  , vsData :: Data r -- ^ data
  -- , vsMarks :: Marks -- ^ marks
  } deriving  (Eq, Show, Generic)
-- instance A.ToJSON r => A.ToJSON (VSpec r) where
--   toJSON (VSpec tm lm w h p _ axs ds mks) =
--     A.object $ [
--         "width" .= w
--       , "height" .= h
--       , "padding" .= p
--       , "$schema" .= schema 5
--       -- , "scales" .= scs
--       , "axes" .= axs
--       , "data" .= ds
--       , "marks" .= mks
--       ] ++ opts where
--     opts = maybeSection "title" tm ++
--            maybeSection "legend" lm

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

data TFrame = TFGroup | TFBounds deriving (Eq, Show, Generic)
instance A.ToJSON TFrame where
  toJSON = \case
    TFGroup -> "group"
    TFBounds -> "bounds"

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

mkDataJson :: String -> [r] -> Data r
mkDataJson dn rs = Data $ M.singleton dn $ DsJSON rs

lookupData :: String -> Data a -> Maybe (DataSource a)
lookupData dn (Data dnm) = M.lookup dn dnm




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

-- | V. axis types
data XAxisType = HATop | HABottom deriving (Eq, Show, Generic)
instance A.ToJSON XAxisType where
  toJSON = \case
    HATop -> "top"
    HABottom -> "bottom"
data YAxisType = VALeft | VARight deriving (Eq, Show, Generic)
instance A.ToJSON YAxisType where
  toJSON = \case
    VALeft -> "left"
    VARight -> "right"




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
