{-# language OverloadedStrings, DeriveGeneric #-}
module Main where

import GHC.Generics

import qualified Data.Aeson as A
import qualified Data.Aeson.Encode.Pretty as A (encodePretty)

import RigelViz
import Lucid (Html(..), renderToFile)
import Lucid.VegaLite (mkVegaHtml)

import qualified Data.Colour as C
import qualified Data.Colour.Names as C
import qualified Data.Colour.SRGB as C (sRGB24show)

import qualified Data.ByteString.Lazy.Char8 as BS (unpack)


-- main = putStrLn "hello!"

main = renderToFile "heatmap.html" $ mkVegaHtml $ A.toJSON vls1


data V3 a = V3 { v3x :: a, v3y :: a, v3z :: a } deriving (Eq, Show, Generic)
instance A.ToJSON a => A.ToJSON (V3 a)

dats :: [V3 Double]
dats = [V3 x y (f x y) | x <- xs, y <- ys] where
  xs = map (/10) [0, 1 .. 20]
  ys = xs
  f x y = sin $ 2 * pi * sqrt (x ** 2 + y ** 2)

vls1 :: VLSpec (V3 Double)
vls1 = vegaLiteSpec 400 400 [
  layer MRect (DataJSON dats) $
      posEnc X "v3x" Ordinal <>
      posEnc Y "v3y" Ordinal  <>
      colourEnc "v3z" Quantitative <>
      sizeEnc "v3z" Quantitative
      ]  


vls0 :: VLSpec TestValue
vls0 =
  vegaLiteSpec 400 300  [
    layer MCircle (DataJSON testVs) (
       posEnc X "tv" Nominal <>
       posEnc Y "tvb" Quantitative <>
       colourEnc "tvb" Quantitative <>
       sizeEnc "tvb" Quantitative
       )
    ]

data T = A | B | C deriving (Eq, Show, Generic)
instance A.ToJSON T
data TestValue = TV { tv :: T, tvb :: Double } deriving (Eq, Show, Generic)
instance A.ToJSON TestValue

testVs :: [TestValue]
testVs = [TV A 3.2, TV B 5.4, TV A 2.2, TV A 6.7, TV B 4.9]
