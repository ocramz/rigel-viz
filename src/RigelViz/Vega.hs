{-# language DeriveGeneric, DeriveDataTypeable, LambdaCase, OverloadedStrings, CPP #-}
-- {-# language GADTs #-}
{-# language DeriveFunctor #-}
{-# language GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
----------------------------------------------------------------
-- |
-- Module      :  RigelViz.Vega
-- Description :  Bindings for 'vega'
-- Copyright   :  (c) Marco Zocca (2019)
-- License     :  MIT
-- Maintainer  :  ocramz
-- Stability   :  experimental
-- Portability :  GHC
--
-----------------------------------------------------------------
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

import Control.Monad.State

import RigelViz.Vega.Types

import Prelude hiding (lookup)

#if MIN_VERSION_base(4,9,0)
import Data.Semigroup
#endif


{- |
A plot can be seen as a mapping between data features and features of visual marks. For example:

  * line chart : data.x -> X, data.y -> Y
  * heatmap    : data.x -> X, data.y -> Y, data.z -> COLOUR
-}





-- data Internal r = Internal {
--     internalData :: M.Map String (Data r)
--   , internalScales :: M.Map String Scale
--   }


data Supply r = Supply { getSupply :: [Int], items :: M.Map String r } deriving (Show, Functor)

intSupply :: Supply r
intSupply = Supply [0 ..] M.empty

-- appendWith :: Ord n => (i -> n) -> a -> Supply i n a -> Supply i n a
appendWith :: (Int -> String) -> r -> Supply r -> Supply r
appendWith f x (Supply (n:ns) mm) = Supply ns mm' where
  mm' = M.insert (f n) x mm

appendWithPrefix :: String -> r -> Supply r -> Supply r
appendWithPrefix p = appendWith (\n -> mconcat [p, show n])  



newtype App r a = App { unApp :: State (Supply r) a }
   deriving (Functor, Applicative, Monad, MonadState (Supply r))

withSupply :: (Int -> String) -> r -> App r ()
withSupply f x = modify (appendWith f x)

withSupply' :: String -> r -> App r ()
withSupply' s x = modify (appendWithPrefix s x)
  
runApp :: App r a -> a
runApp a = evalState (unApp a) intSupply



testApp = M.toList <$> do
  let t = withSupply' "tick"
  _ <- t 42
  t 312
  gets items


















-- * Scale



-- instance A.ToJSON Scales where
--   toJSON scs = array $ M.foldlWithKey insf [] $ unScales scs where
--     insf acc scName (Scale sty sd) = wrap (["name" .= scName, "domain" .= sd] ++ opts) ++ acc
--       where
--         opts = case sty of
--           STLinear r zb  -> [
--               "type"    .= string "linear"
--             , "range"   .= r
--             , "zero"    .= zb 
--             ]
--           STBand pad r   -> [
--               "type"    .= string "band"
--             , "range" .= r
--             , "padding" .= pad
--             ]
--           STColours cols -> case cols of
--             ColScheme cs -> [
--                 "type" .= string "linear"
--               , "range" .= A.object ["scheme" .= cs]
--               ]
--             ColValues cvs -> [
--                 "type" .= string "ordinal"
--               , "range" .= map (string . T.pack . C.sRGB24show) cvs
--               ]




-- | Data encoding channels 
newtype EncChans v = EncChans { unSM :: M.Map String (Channel v) } deriving (Show)

instance Semigroup (EncChans v) where
  (EncChans sm1) <> (EncChans sm2) = EncChans $ sm1 <> sm2


-- MRectC String MGeomEnc MGeomEnc MGeomEnc MGeomEnc -- ^ from.data, xc, yc, w, h
-- MRectV String MGeomEnc MGeomEnc MGeomEnc MGeomEnc -- ^ from.data, x, y, width, y2

-- | Encoding channels for marks
--
-- mid-level representation: each 'Channel' will be interpreted in a (scale, mark channel) pair
data Channel v =
    CLinearField  DomP PlotRange Bool String  -- "x": {"scale": "scx", "field": "a"}          -- linear  
  | CLinearValue  DomP PlotRange Bool v  -- "y": {"scale": "scy", "value": 0}                 -- linear
  | CBand         DomP PlotRange Double Double Double -- "width": {"scale": "scw", "band": 1} -- band
  | CColourScheme DomP ColourScheme                                                 -- linear or band
  | COrdinalCol   DomP [C.Colour Double]                                              -- ordinal   
  | CConst v -- "fillOpacity": {"value": 0.5}
  deriving (Show)

-- | 'scale' domain properties
data DomP = DomP {
    cpDomainData :: String   -- ^ domain.data (dataset name)
  , cpDomainField :: String  -- ^ domain.field (field the scale is built with)
  } deriving (Show)


-- 1. [channel] -> [(scale, mark encoding)]
-- 2. [scale]   -> [Maybe axis]



-- encode a Channel into a Scale
encodeChannel n = \case
  CLinearField (DomP dd df) pr z rf ->
    ScaleEnc n "linear" dd df $ M.fromList [("range", A.toJSON pr), ("zero", A.toJSON z)]
  CLinearValue (DomP dd df) pr z val ->
    ScaleEnc n "linear" dd df $ M.fromList []

-- | Primitive mark type
data MarkPrimitive = MPRect | MPSymbol deriving (Show, Generic)
instance A.ToJSON MarkPrimitive where
  toJSON = \case
    MPRect -> "rect"
    MPSymbol -> "symbol"

-- | Mark encoding
data MarkEnc =
    MarkEncPrim MarkPrimitive String (M.Map T.Text A.Value)
  | MarkEncGroup [MarkEnc]
  deriving (Show, Generic)

instance A.ToJSON MarkEnc where
  toJSON = \case
    MarkEncGroup mks ->
      A.object [
        "type" .= string "group"
      , "marks" .= A.toJSON mks
      ]
    MarkEncPrim mtype mdf mfs -> 
      A.object [
          "type" .= mtype
        , "from" .= A.object ["data" .= mdf]
        , "encode" .= A.object [
            "enter" .= A.toJSON mfs
            ]
        ] 

-- | low-level Scale encoding
data ScaleEnc = ScaleEnc {
    seName :: String  -- name
  , seType :: String  -- type
  , seDomData :: String  -- domain.data
  , seDomField :: String -- domain.field  
  , seExtraFields :: M.Map T.Text A.Value  -- <additional optional fields>
  } deriving (Show, Generic)

-- | 'ScaleEnc' has a 1:1 JSON encoding
instance A.ToJSON ScaleEnc where
  toJSON (ScaleEnc sn sty sdd sdf sef) = A.object $ [
      "name" .= sn
    , "type" .= sty
    , "domain" .= A.object ["data" .= sdd, "field" .= sdf]
                                               ] ++ M.toList sef


data PlotRange = Width | Height deriving (Eq, Show, Generic)
instance A.ToJSON PlotRange where
  toJSON = \case
    Width -> "width"
    Height -> "height"

-- | Colour schemes for e.g. heatmaps
data ColourScheme = CSPlasma deriving (Eq, Show, Generic)
instance A.ToJSON ColourScheme where
  toJSON = \case
    CSPlasma -> "plasma"












-- * Mark

data Mark v =
    MRect String (EncChans v)   -- "from", "encode.enter"
  | MSymbol String (EncChans v)
  | MGroup [Mark v]
  deriving (Show, Generic)





-- newtype Marks = Marks [Mark] deriving (Eq, Show, Generic)
-- instance A.ToJSON Marks where
--   toJSON (Marks mks) = A.object ["marks" .= map A.toJSON mks]

-- -- | Mark geometry encoding
-- data Mark =
--     MRectC String MGeomEnc MGeomEnc MGeomEnc MGeomEnc -- ^ from.data, xc, yc, w, h
--   | MRectV String MGeomEnc MGeomEnc MGeomEnc MGeomEnc -- ^ from.data, x, y, width, y2
--   | MSymbol String MarkSymbolShape MGeomEnc MGeomEnc MGeomEnc -- ^ from.data, shape, x, y, size
--   | MGroup [Mark] -- ^ "group"
--   deriving (Eq, Show, Generic)

-- instance A.ToJSON Mark where
--   toJSON = \case 
--     MGroup mges  -> A.object [
--       "type" .= string "group",
--       "marks" .= map A.toJSON mges
--                              ]
--     MRectC dfrom xc yc w h -> A.object [
--         "type" .= string "rect"
--       , "from" .= A.object ["data" .= dfrom]
--       , "encode" .= A.object [
--           "enter" .=
--             A.object [
--                 "xc" .= xc
--               , "yc" .= yc
--               , "width" .= w
--               , "height" .= h 
--               ]
--           ]
--       ]
--     MRectV dfrom x y w y2 -> A.object [
--         "type" .= string "rect"
--       , "from" .= A.object ["data" .= dfrom]
--       , "encode" .= A.object [
--           "enter" .=
--             A.object [
--                 "x" .= x
--               , "y" .= y
--               , "width" .= w
--               , "y2" .= y2
--               ]
--           ]
--       ]
--     MSymbol dfrom msh x y sz -> A.object [
--         "type" .= string "symbol"
--       , "from" .= A.object ["data" .= dfrom]
--       , "encode" .= A.object [
--           "enter" .=
--             A.object [
--                 "shape" .= msh
--               , "x" .= x
--               , "y" .= y
--               , "size" .= sz
--               ]
--           ]
--       ]









-- | Shapes for the "symbol" Mark
data MarkSymbolShape =
    MSSCircle  -- ^ "circle"
  | MSSCross   -- ^ "cross"
  deriving (Eq, Show, Generic)
instance A.ToJSON MarkSymbolShape where
  toJSON = \case
    MSSCircle -> "circle"
    MSSCross  -> "cross"






