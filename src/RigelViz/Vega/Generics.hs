{-# language DeriveGeneric #-}
{-# language LambdaCase #-}
{-# language DefaultSignatures #-}
{-# language FlexibleInstances #-}
{-# language GADTs, FlexibleContexts #-}
----------------------------------------------------------------
-- |
-- Module      :  RigelViz.Vega.Generics
-- Description :  Generic encoding of algebraic types
-- Copyright   :  (c) Marco Zocca (2019)
-- License     :  MIT
-- Maintainer  :  ocramz fripost org
-- Stability   :  experimental
-- Portability :  GHC
--
-- Generic encoding of algebraic datatypes, using @generics-sop@
--
-- The 'gPickDouble','gPickText' functions can be used to index
-- into both anonymous product types and record types.
--
-- NB : currently it is not possible to index into nested or recursive types (i.e. only "flat" records are supported)
--
-- The examples will use the following type declarations :
--
-- @
-- data C = C Int Float deriving (Show, G.Generic)
-- instance Generic C
-- 
-- -- NB : the generic encoding of D is identical to that of C
-- data D = D { d1 :: Int, d2 :: Float } deriving (Show, G.Generic)
-- instance Generic D
-- @
-----------------------------------------------------------------
module RigelViz.Vega.Generics (
  -- * Index into records 
  gPickDouble
  , gPickText
  -- ** Helper typeclasses
  , HasDouble, HasText
  ) where

-- import Data.Char (toLower, ord)
import Data.Char (toLower)
-- import Data.Proxy (Proxy(..))
import qualified GHC.Generics as G

-- generics-sop
-- import Generics.SOP (All(..), All2, Generic(..), SOP(..), NS(..), NP(..), I(..), HasDatatypeInfo(..))
import Generics.SOP (All(..), All2, Generic(..), SOP(..), NS(..), NP(..), I(..))
-- text
import qualified Data.Text as T (Text, pack)




-- | Can a record field be cast as a Double ?
class HasDouble t where
  hasDouble :: t -> Double
instance HasDouble Int where hasDouble = fromIntegral
instance HasDouble Float where hasDouble = fromRational . toRational
instance HasDouble Double where hasDouble = id
-- instance HasDouble Char where hasDouble = fromIntegral . ord -- ewwww
instance HasDouble a => HasDouble (Maybe a) where
  hasDouble = \case
    Nothing -> 0
    Just x -> hasDouble x

-- class HasInt t where
--   hasInt :: t -> Int
--   default hasInt :: RealFrac t => t -> Int
--   hasInt = floor
-- instance HasInt Int where hasInt = id
-- instance HasInt Char where hasInt = ord

-- | Can a record field be interpreted as a Text string ? This one is easy, so we provide a default instance via Show
class HasText t where
  hasText :: t -> T.Text
  default hasText :: Show t => t -> T.Text
  hasText = T.pack . map toLower . show
instance HasText Int
instance HasText Bool
instance HasText Double
instance HasText Float
instance HasText String where
  hasText = T.pack


-- | Index into a product type and render the field as a Text string
--
-- >>> gPickText 0 $ C 42 6.6
-- Just "42"
-- 
-- >>> gPickText 1 $ D 42 6.6
-- Just "6.6"
gPickText :: (Generic a, All2 HasText (Code a)) =>
              Int -> a -> Maybe T.Text
gPickText i = gPickTextS i . from 

gPickTextS :: All2 HasText xss => Int -> SOP I xss -> Maybe T.Text
gPickTextS i0 = \case
  SOP (Z xs)  -> gPickTextP i0 xs
  SOP (S xss) -> gPickTextS i0 (SOP xss)

gPickTextP :: All HasText xs =>
                Int -- ^ index into the tuple
             -> NP I xs
             -> Maybe T.Text
gPickTextP i0 = go 0
  where
    go :: All HasText xs => Int -> NP I xs -> Maybe T.Text
    go i pr = case pr of
      Nil -> Nothing
      I x :* xs -> if i == i0
        then Just $ hasText x
        else go (succ i) xs



-- | Index into a product type and cast the field as a Double
-- 
-- >>> gPickDouble 0 $ C 42 6.6
-- Just 42.0
-- 
-- >>> gPickDouble 1 $ D 42 6.6
-- Just 6.6
gPickDouble :: (Generic a, All2 HasDouble (Code a)) =>
              Int -> a -> Maybe Double
gPickDouble i = gPickDoubleS i . from 

gPickDoubleS :: All2 HasDouble xss => Int -> SOP I xss -> Maybe Double
gPickDoubleS i0 = \case
  SOP (Z xs)  -> gPickDoubleP i0 xs
  SOP (S xss) -> gPickDoubleS i0 (SOP xss)

gPickDoubleP :: All HasDouble xs =>
                Int -- ^ index into the tuple
             -> NP I xs
             -> Maybe Double
gPickDoubleP i0 = go 0
  where
    go :: All HasDouble xs => Int -> NP I xs -> Maybe Double
    go i pr = case pr of
      Nil -> Nothing
      I x :* xs -> if i == i0
        then Just $ hasDouble x
        else go (succ i) xs


-- test data types

data A = A1 Bool | A2 A Int 
  deriving (Show, G.Generic)
instance Generic A

-- λ> from $ B 42 True "mooo"
-- SOP (Z (I 42 :* I True :* I "mooo" :* Nil))
data B = B Int Bool String deriving (Show, G.Generic)
instance Generic B

-- λ> from $ C 42 6.6
-- SOP (Z (I 42 :* I 6.6 :* Nil))
data C = C Int Double deriving (Show, G.Generic)
instance Generic C

-- NB : the generic encoding of D is identical to that of C
data D = D { d1 :: Int, d2 :: Double } deriving (Show, G.Generic)
instance Generic D