{-# language OverloadedStrings, ExtendedDefaultRules #-}
module Lucid.VegaLite where

import Graphics.Vega.VegaLite
import Lucid
import Lucid.PreEscaped (scriptSrc)

import qualified Data.Aeson as A

import qualified Data.Text as T
import qualified Data.Text.Encoding as T (decodeUtf8)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

import Data.Monoid

vegaCDN, vegaLiteCDN, vegaEmbedCDN :: Monad m => HtmlT m ()
vegaCDN = scriptSrc "https://cdn.jsdelivr.net/npm/vega@4"
vegaLiteCDN = scriptSrc "https://cdn.jsdelivr.net/npm/vega-lite@2.0.0"
vegaEmbedCDN = scriptSrc "https://cdn.jsdelivr.net/npm/vega-embed@3"

doc :: VegaLite -> Html ()
doc vl = doctypehtml_ $ html_ $ do
  meta_ [charset_ "UTF-8"]
  head_ $ do
    vegaCDN
    vegaLiteCDN
    vegaEmbedCDN
  with div_ [id_ "vis"] ""
  body_ $ 
    script_ $ T.decodeUtf8 $ LBS.toStrict ("const spec =" <> A.encode (fromVL vl) <> "; vegaEmbed('#vis', spec).then(result => console.log(result)).catch(console.warn);" :: LBS.ByteString) 

moo = renderToFile "test.html" (doc t1)


--  <meta charset="UTF-8"> 


{-
.then(result => console.log(result))
      .catch(console.warn);

<!DOCTYPE html>
<html>
<head>
  <!-- Import Vega 4 & Vega-Lite 3 (does not have to be from CDN) -->
  <script src="https://cdn.jsdelivr.net/npm/vega@[VERSION]"></script>
  <script src="https://cdn.jsdelivr.net/npm/vega-lite@[VERSION]"></script>
  <!-- Import vega-embed -->
  <script src="https://cdn.jsdelivr.net/npm/vega-embed@[VERSION]"></script>
</head>
<body>

<div id="vis"></div>

<script type="text/javascript">
  var spec = "https://raw.githubusercontent.com/vega/vega/master/docs/examples/bar-chart.vg.json";
  vegaEmbed('#vis', spec).then(function(result) {
    // Access the Vega view instance (https://vega.github.io/vega/docs/api/view/) as result.view
  }).catch(console.error);
</script>
</body>
</html>
-}


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
