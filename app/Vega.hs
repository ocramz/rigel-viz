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

import qualified RigelViz.Vega as Vega


main = putStrLn "hello!"




