{-# language DeriveGeneric #-}
module RigelViz.Util where

import GHC.Generics

import qualified Data.Aeson as A
import qualified Data.Aeson.Encode.Pretty as A (encodePretty)

import qualified Data.ByteString.Lazy.Char8 as BS (unpack)





data V3 a = V3 { v3x :: a, v3y :: a, v3z :: a } deriving (Eq, Show, Generic)
instance A.ToJSON a => A.ToJSON (V3 a)

dats :: [V3 Double]
dats = [V3 x y (f x y) | x <- xs, y <- ys] where
  xs = map (/2) [0, 1 .. 20]
  ys = xs
  f x y = sin $ 2 * pi * sqrt (x ** 2 + y ** 2)



-- | encode and pretty-print a JSON blob
pprint :: A.ToJSON a => a -> IO ()
pprint = putStrLn . BS.unpack . A.encodePretty
