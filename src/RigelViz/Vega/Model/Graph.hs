{-# language LambdaCase #-}
{-# language TemplateHaskell #-}
{-# language DeriveGeneric #-}
{-# language DeriveFunctor #-}
{-# language GeneralizedNewtypeDeriving, DeriveTraversable #-}
module RigelViz.Vega.Model.Graph where

import Data.Foldable (Foldable(..))
import qualified GHC.Generics as G (Generic(..))

-- aeson
import qualified Data.Aeson as A (ToJSON(..))
-- algebraic-graphs
import Algebra.Graph (Graph(..))
import Algebra.Graph.AdjacencyMap (AdjacencyMap, empty, vertex, edge, connect, overlay, hasVertex, hasEdge, preSet, postSet)
-- colour
import qualified Data.Colour as C
import qualified Data.Colour.SRGB as C (sRGB24show)
-- containers
import qualified Data.Map as M (Map, fromList, insert, empty, lookup)
import qualified Data.Set as S (Set)
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



data Node d =
     Dataset d
   | Scale
   | Mark
