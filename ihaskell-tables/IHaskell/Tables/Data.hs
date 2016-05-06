-- |
-- Module      : IHaskell.Tables.Data
-- Copyright   : (c) Justus Sagemüller 2016
-- License     : GPL v3
-- 
-- Maintainer  : (@) sagemueller $ geo.uni-koeln.de
-- Stability   : experimental
-- Portability : portable
-- 

{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE FlexibleInstances #-}

module IHaskell.Tables.Data (Table, tabular, TabShow) where


import IHaskell.Display
import IHaskell.Display.Blaze

import qualified Text.Blaze.Html4.Strict as H
import qualified Text.Blaze.Html4.Strict.Attributes as HA


-- | A tabular view of Haskell data.
data Table d = Table { getTableContent :: d
                     , tableLegend :: Maybe (TSDLegend d)
                     }

tabular :: d -> Table d
tabular x = Table x Nothing

tableWithLegend :: TabShow d => TSDLegend d -> d -> Table d
tableWithLegend legend x = Table x $ pure legend

type HTML = H.Html

class TabShow s where
  type TSDLegend s :: *
  type TSDLegend s = String
  type SharedPrecomputation s :: *
  type SharedPrecomputation s = ()
  precompute :: s -> SharedPrecomputation s
  showAsTable :: Maybe (TSDLegend s) -> SharedPrecomputation s -> s
                   -> HTML  -- might become Javascript instead.

instance TabShow Int where
  precompute _ = ()
  showAsTable _ _ i = H.toHtml i
instance TabShow String where
  precompute _ = ()
  showAsTable _ _ i = H.toHtml i

instance (Show s, TabShow s) => IHaskellDisplay (Table s) where
  display (Table i l)
      = display $ showAsTable l (precompute i) i
  


