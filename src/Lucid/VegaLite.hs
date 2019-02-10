{-# language OverloadedStrings #-}
module Lucid.VegaLite where

import Graphics.Vega.VegaLite
import Lucid

import qualified Data.Aeson as A





-- example data

t1 :: VegaLite
t1 = toVegaLite [description desc, background "white", dat [], mark Bar barOpts, enc []] where
  desc = "A very exciting bar chart"

  dat = dataFromRows [Parse [("start", FoDate "%Y-%m-%d")]]
          . dataRow [("start", Str "2011-03-25"), ("count", Number 23)]
          . dataRow [("start", Str "2011-04-02"), ("count", Number 45)]
          . dataRow [("start", Str "2011-04-12"), ("count", Number 3)]

  barOpts = [MOpacity 0.4, MColor "teal"]

  enc = encoding
          . position X [PName "start", PmType Temporal, PAxis [AxTitle "Inception date"]]
          . position Y [PName "count", PmType Quantitative]
