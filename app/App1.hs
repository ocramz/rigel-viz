{-# language OverloadedStrings, DeriveGeneric #-}
module Main where

import GHC.Generics

import qualified Data.Aeson as A
import qualified Data.Aeson.Encode.Pretty as A (encodePretty)


import Lucid (Html(..), renderToFile)
import Lucid.VegaLite (mkVegaHtml)

import qualified Data.Colour as C
import qualified Data.Colour.Names as C
import qualified Data.Colour.SRGB as C (sRGB24show)

import qualified Data.ByteString.Lazy.Char8 as BS (unpack)

import RigelViz
import Utils



-- main = putStrLn "hello!"

main = renderToFile "cropped.html" $ mkVegaHtml $ A.toJSON vls2



-- | line chart with trimmed domain and range

data WL = WL { wl1 :: Double , wl2 :: Double} deriving (Eq, Show, Generic)
instance A.ToJSON WL

vls2 :: VLSpec Double WL
vls2 = vegaLiteSpec 400 400 [
  layer MLine (DataJSON wls) $
     posEnc X "wl1" Quantitative bx <>
     posEnc Y "wl2" Quantitative by
                            ]
  where
      bx = bounds 300 450
      by = bounds 0.5 5

wls = [WL 250 1, WL 300 2, WL 320 3.5, WL 450 1.2, WL 500 2.4]



-- | heatmap 



vls1 :: VLSpec Double (V3 Double)
vls1 = vegaLiteSpec 400 400 [
  layer MRect (DataJSON dats) $
      posEnc X "v3x" Quantitative bx <>
      posEnc Y "v3y" Quantitative  by <>
      colourEnc "v3z" Quantitative bz
      ]
  where
    bx = range $ map (/2) [0, 1 .. 20] -- bounds 0 20
    by = bx
    bz = bounds (negate 1) 1
  

-- | scatter plot

-- vls0 :: VLSpec TestValue
vls0 =
  vegaLiteSpec 400 300 [
    layer MCircle (DataJSON testVs) (
       posEnc X "tv" Nominal bx  <>
       posEnc Y "tvb" Quantitative by <>
       colourEnc "tvb" Quantitative bz <>
       sizeEnc "tvb" Quantitative bz
       )
    ]
    where
    bx = range $ map (/10) [0, 1 .. 20] -- bounds 0 20
    by = bx
    bz = bounds (negate 1) 1

data T = A | B | C deriving (Eq, Show, Generic)
instance A.ToJSON T
data TestValue = TV { tv :: T, tvb :: Double } deriving (Eq, Show, Generic)
instance A.ToJSON TestValue

testVs :: [TestValue]
testVs = [TV A 3.2, TV B 5.4, TV A 2.2, TV A 6.7, TV B 4.9]
