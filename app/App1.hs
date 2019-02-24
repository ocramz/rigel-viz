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

main = renderToFile "heatmap.html" $ mkVegaHtml vls1


data V3 a = V3 { v3x :: a, v3y :: a, v3z :: a } deriving (Eq, Show, Generic)
instance A.ToJSON a => A.ToJSON (V3 a)

dats :: [V3 Double]
dats = [V3 x y (f x y) | x <- xs, y <- ys] where
  xs = [0, 0.2 .. 2]
  ys = xs
  f x y = sin $ sqrt (x ** 2 + y ** 2)

vls1 = A.toJSON $ VLSpec 400 400 (DataJSON dats) [
  LayerMD (Mark MCircle) $
      posXEnc "v3x" Quantitative <>
      posYEnc "v3y" Quantitative  <>
      colourEnc "v3z" Quantitative <>
      sizeEnc "v3z" Quantitative
                                                 ]  


vls0 :: A.Value
vls0 =
  A.toJSON $ VLSpec 400 300 (DataJSON testVs) $ [
    LayerMD (Mark MCircle ) (
       posXEnc "tv" Nominal <>
       posYEnc "tvb" Quantitative <>
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
