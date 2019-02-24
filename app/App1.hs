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

main = renderToFile "asdf.html" $ mkVegaHtml vls0



vls0 :: A.Value
vls0 =
  A.toJSON $ VLSpec 400 300 (DataJSON testVs) $ [
    LayerMD (Mark MCircle ) (
       Enc (EncMD "tv" ETNominal)
           (EncMD "tvb" ETQuantitative)
           (Just (ColourEnc (EncMD "tvb" ETQuantitative)))
           Nothing
           Nothing
           (Just (SizeEnc (EncMD "tvb" ETQuantitative)))
       )
    ]

data T = A | B | C deriving (Eq, Show, Generic)
instance A.ToJSON T
data TestValue = TV { tv :: T, tvb :: Double } deriving (Eq, Show, Generic)
instance A.ToJSON TestValue

testVs :: [TestValue]
testVs = [TV A 3.2, TV B 5.4, TV A 2.2, TV A 6.7, TV B 4.9]
