{-# language DeriveGeneric #-}
{-# language LambdaCase #-}
{-# language DefaultSignatures #-}
{-# language FlexibleInstances #-}
{-# language GADTs, FlexibleContexts #-}
{-# language ScopedTypeVariables #-}
----------------------------------------------------------------
-- |
-- Module      :  RigelViz.Vega.Generics
-- Description :  Generic encoding of algebraic types
-- Copyright   :  (c) Marco Zocca (2019)
-- License     :  MIT
-- Maintainer  :  ocramz 
-- Stability   :  experimental
-- Portability :  GHC
--
-- Generic encoding of algebraic datatypes and their metadata (e.g. record field names), using @generics-sop@
--
-- NB : currently it is not possible to index into nested or recursive types (i.e. only "one level deep" records are supported)
--
-- The examples will use the following type declarations :
--
-- @
-- data A = A Int Double deriving (Show, 'G.Generic')
-- instance 'Generic' A
-- instance 'HasDatatypeInfo' A
--
-- -- NB : the generic encoding of B is identical to that of A, the only difference is that fields are named
-- data B = B { b1 :: Int, b2 :: Double } deriving (Show, G.Generic)
-- instance Generic B
-- instance HasDatatypeInfo B
--
-- -- General case : a sum type with named fields
-- data C = C1 {c11 :: Int, c12 :: Double} | C2 {c21 :: Double} deriving (Show, G.Generic)
-- instance Generic C
-- instance HasDatatypeInfo C
-- @
-----------------------------------------------------------------
module RigelViz.Vega.Generics (sopFieldNames) where

-- import Data.Char (toLower, ord)
import Data.Char (toLower)
import Data.Foldable (Foldable(..))
import Data.Proxy (Proxy(..))
import qualified GHC.Generics as G

-- containers
import qualified Data.Map as M (Map, fromList)
import qualified Data.IntMap as IM (IntMap, fromList)
-- generics-sop
-- import Generics.SOP (All(..), All2, Generic(..), SOP(..), NS(..), NP(..), I(..), HasDatatypeInfo(..))
import Generics.SOP (All(..), All2, AllN, HAp, Prod, Generic(..), SOP(..), NS(..), NP(..), hcmap, hcfoldMap, hcollapse, mapIK, I(..), K(..), HasDatatypeInfo(..), DatatypeInfo(..), ConstructorInfo(..), constructorInfo, FieldInfo(..), fieldName, DatatypeName, FieldName, SListI(..))
-- text
import qualified Data.Text as T (Text, pack)

-- | All record field names for each variant of a sum type
--
-- >>> sopFieldNames (A 42 0.2)
-- fromList []
--
-- >>> sopFieldNames (B 42 0.2)
-- fromList [("B",["b1","b2"])]
--
-- >>> sopFieldNames (C1 42 0.2)
-- fromList [("C1",["c11","c12"]),("C2",["c21"])]
sopFieldNames :: forall t . HasDatatypeInfo t => t -> M.Map DatatypeName [FieldName]
sopFieldNames _ = constructorFields (constructorInfo (datatypeInfo (Proxy :: Proxy t)))

constructorFields :: NP ConstructorInfo xs -> M.Map DatatypeName [FieldName]
constructorFields cstr = M.fromList (go cstr)
  where
    go :: NP ConstructorInfo xs -> [(DatatypeName, [FieldName])]
    go cs = case cs of
      Record dtn finfo :* finfs -> (dtn, fieldNames finfo) : go finfs
      _ -> []
    fieldNames :: NP FieldInfo a -> [FieldName]
    fieldNames pr = case pr of
      Nil -> []
      FieldInfo f :* fs -> f : fieldNames fs

--
{-
Does a type have a field with a given name?
-}

-- hasField :: forall t . HasDatatypeInfo t => String -> t -> Bool
-- hasField f0 _ =
--   matchConstr f0 (constructorInfo (datatypeInfo (Proxy :: Proxy t)))
--   where
--    matchConstr :: String -> NP ConstructorInfo xs -> Bool
--    matchConstr f0 cstr = case cstr of
--      Record _ finfo :* finfs -> hasFieldName f0 finfo || matchConstr f0 finfs
--      _ -> False 

-- -- | Does a given record contain the given field?
-- hasFieldName :: String -> NP FieldInfo xs -> Bool
-- hasFieldName f0 pr = case pr of
--   Nil -> False
--   FieldInfo f :* fs -> (f == f0) || hasFieldName f0 fs



-- test data types

data A = A1 Bool | A2 A Int 
  deriving (Show, G.Generic)
instance Generic A
instance HasDatatypeInfo A

-- λ> from $ B 42 True "mooo"
-- SOP (Z (I 42 :* I True :* I "mooo" :* Nil))
data B = B Int Bool String deriving (Show, G.Generic)
instance Generic B
instance HasDatatypeInfo B

-- λ> from $ C 42 6.6
-- SOP (Z (I 42 :* I 6.6 :* Nil))
data C = C Int Double deriving (Show, G.Generic)
instance Generic C
instance HasDatatypeInfo C

-- NB : the generic encoding of D is identical to that of C
data D = D { d1 :: Int, d2 :: Double } deriving (Show, G.Generic)
instance Generic D
instance HasDatatypeInfo D

data E = E1 {e11 :: Int, e12 :: Double} | E2 {e21 :: Double} deriving (Show, G.Generic)
instance Generic E
instance HasDatatypeInfo E






-- -- | Can a record field be cast as a Double ?
-- class HasDouble t where
--   hasDouble :: t -> Double
-- instance HasDouble Int where hasDouble = fromIntegral
-- instance HasDouble Float where hasDouble = fromRational . toRational
-- instance HasDouble Double where hasDouble = id
-- -- instance HasDouble Char where hasDouble = fromIntegral . ord -- ewwww
-- instance HasDouble a => HasDouble (Maybe a) where
--   hasDouble = \case
--     Nothing -> 0
--     Just x -> hasDouble x

-- -- class HasInt t where
-- --   hasInt :: t -> Int
-- --   default hasInt :: RealFrac t => t -> Int
-- --   hasInt = floor
-- -- instance HasInt Int where hasInt = id
-- -- instance HasInt Char where hasInt = ord

-- -- | Can a record field be interpreted as a Text string ? This one is easy, so we provide a default instance via Show
-- class HasText t where
--   hasText :: t -> T.Text
--   default hasText :: Show t => t -> T.Text
--   hasText = T.pack . map toLower . show
-- instance HasText Int
-- instance HasText Bool
-- instance HasText Double
-- instance HasText Float
-- instance HasText String where
--   hasText = T.pack


-- -- | Index into a product type and render the field as a Text string
-- --
-- -- >>> gPickText 0 $ B 42 True "moo"
-- -- Just "42"
-- --
-- -- >>> gPickText 1 $ B 42 True "moo"
-- -- Just "true"
-- gPickText :: (Generic a, All2 HasText (Code a)) =>
--               Int -- ^ index into the tuple
--           -> a -> Maybe T.Text
-- gPickText i = gPickTextS i . from 

-- gPickTextS :: All2 HasText xss => Int -> SOP I xss -> Maybe T.Text
-- gPickTextS i0 = \case
--   SOP (Z xs)  -> gPickTextP i0 xs
--   SOP (S xss) -> gPickTextS i0 (SOP xss)

-- gPickTextP :: All HasText xs =>
--                 Int 
--              -> NP I xs
--              -> Maybe T.Text
-- gPickTextP i0 = go 0
--   where
--     go :: All HasText xs => Int -> NP I xs -> Maybe T.Text
--     go i pr = case pr of
--       Nil -> Nothing
--       I x :* xs -> if i == i0
--         then Just $ hasText x
--         else go (succ i) xs






-- -- | Index into a product type and cast the field as a Double
-- -- 
-- -- >>> gPickDouble 0 $ C 42 6.6
-- -- Just 42.0
-- -- 
-- -- >>> gPickDouble 1 $ D 42 6.6
-- -- Just 6.6
-- -- 
-- -- >>> gPickDouble 5 $ C 42 6.6
-- -- Nothing
-- gPickDouble :: (Generic a, All2 HasDouble (Code a)) =>
--                Int -- ^ index into the tuple
--             -> a -> Maybe Double
-- gPickDouble i = gPickDoubleS i . from 

-- gPickDoubleS :: All2 HasDouble xss => Int -> SOP I xss -> Maybe Double
-- gPickDoubleS i0 = \case
--   SOP (Z xs)  -> gPickDoubleP i0 xs
--   SOP (S xss) -> gPickDoubleS i0 (SOP xss)

-- gPickDoubleP :: All HasDouble xs =>
--                 Int -- ^ index into the tuple
--              -> NP I xs
--              -> Maybe Double
-- gPickDoubleP i0 = go 0
--   where
--     go :: All HasDouble xs => Int -> NP I xs -> Maybe Double
--     go i pr = case pr of
--       Nil -> Nothing
--       I x :* xs -> if i == i0
--         then Just $ hasDouble x
--         else go (succ i) xs


