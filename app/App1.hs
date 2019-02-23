{-# language OverloadedStrings, DeriveGeneric #-}
module Main where

import GHC.Generics
import qualified Data.Aeson as A

import RigelViz
import Lucid (Html(..), renderToFile)
import Lucid.VegaLite (mkVegaHtml)


main = renderToFile "asdf.html" $ mkVegaHtml vls0

vls0 :: A.Value
vls0 =
  A.toJSON $ VLSpec 400 300 (DataJSON testVs) $
    VSingle MPoint (
       Encs (EncMetadata "tva" ETQuantitative)
            (EncMetadata "tvb" ETQuantitative)
            Nothing
            Nothing
            Nothing 
       )

data TestValue = TV { tva :: Int, tvb :: Double } deriving (Eq, Show, Generic)
instance A.ToJSON TestValue

testVs :: [TestValue]
testVs = [TV 1 3.2, TV 2 5.4]
