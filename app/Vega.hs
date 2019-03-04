{-# language OverloadedStrings, DeriveGeneric, DeriveDataTypeable #-}
module Main where

import GHC.Generics (Generic(..))
import Data.Typeable (Typeable(..), typeRep)

import qualified Data.Aeson as A
import qualified Data.Aeson.Encode.Pretty as A (encodePretty)

import Lucid (Html(..), renderToFile)
import Lucid.VegaLite (mkVegaHtml)

import qualified Data.Colour as C
import qualified Data.Colour.Names as C
import qualified Data.Colour.SRGB as C (sRGB24show)

import qualified Data.ByteString.Lazy.Char8 as BS (unpack)

import RigelViz.Vega
import RigelViz.Util (V3(..), dats, pprint)

main = putStrLn "hello!"


-- vs0 = do
--   scx <- mkScale d0 "v" "v3x" (STLinear Width False)
--   scy <- mkScale d0 "v" "v3y" (STLinear Height False)
--   let sc = scx <> scy
--   undefined
  
  -- vegaSpec Nothing Nothing 400 300 0


d0 :: Data (V3 Double)
d0 = mkDataJson "v" [V3 0 0 0, V3 1 2 3]
