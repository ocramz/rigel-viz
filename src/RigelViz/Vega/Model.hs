{-# LANGUAGE FlexibleContexts #-}
{-# language LambdaCase #-}
{-# language TemplateHaskell #-}
{-# language DeriveGeneric #-}
{-# language DeriveFunctor #-}
{-# language GeneralizedNewtypeDeriving, DeriveTraversable #-}
{-# language UndecidableInstances #-}
-- {-# OPTIONS_GHC -Wno-unused-imports #-}
module RigelViz.Vega.Model where

-- import Control.Applicative (liftA2)
import Data.Foldable (Foldable(..))
import qualified GHC.Generics as G (Generic(..))

-- aeson
import qualified Data.Aeson as A (ToJSON(..))
-- colour
import qualified Data.Colour as C
import qualified Data.Colour.SRGB as C (sRGB24show)
-- containers
import qualified Data.Map as M (Map, fromList, insert, empty, lookup)
-- generics-sop
import Generics.SOP.GGP (GDatatypeInfo)
-- microlens
import Lens.Micro (Lens, Lens', Traversal, Traversal', (&))
import Lens.Micro (ASetter, (.~), (?~)) -- setters
import Lens.Micro ((%~)) -- modifiers
import Lens.Micro (Getting, (^.)) -- getters
import Lens.Micro (traverseOf, traversed, _Just) -- traversals
-- microlens-ghc   -- traversals for 'containers' et al.
import Lens.Micro.GHC (at, ix) -- the respective typeclasses are not exported
-- microlens-mtl
import Lens.Micro.Mtl ((.=), (?=), assign) -- setters
import Lens.Micro.Mtl ((%=), zoom) -- modifiers
import Lens.Micro.Mtl (use) -- getters
-- microlens-th
import Lens.Micro.TH
-- mtl
import Control.Monad.State.Class (MonadState(..), gets)
import Control.Monad.State (State, runState, StateT(..), runStateT)

-- import RigelViz.Vega.Types
-- import RigelViz.Vega.Generics (sopFieldNames)



-- data Plot d =
--   ScatterPlot {
--     spData :: d
--     }
--   | LinePlot {
--     lpData :: d
--              }
--   | HeatmapPlot {
--     hpData :: d
--     } deriving (Eq, Show)

-- renderPlot = \case
--   ScatterPlot d -> undefined




{- plot composition operator ideas :

a <=> b = undefined
u <||> v = undefined 
-}

