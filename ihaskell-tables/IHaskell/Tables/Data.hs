-- |
-- Module      : IHaskell.Tables.Data
-- Copyright   : (c) Justus Sagemüller 2016
-- License     : GPL v3
-- 
-- Maintainer  : (@) sagemueller $ geo.uni-koeln.de
-- Stability   : experimental
-- Portability : portable
-- 

{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE UndecidableInstances #-}

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

data TableLevel = TabListItem | TabAtomic | TabCols | TabRows

class TabShow s where
  type TSDLegend s :: *
  type TSDLegend s = String
  type SharedPrecomputation s :: *
  type SharedPrecomputation s = ()
  precompute :: s -> SharedPrecomputation s
  showAsTable :: Maybe (TSDLegend s) -> SharedPrecomputation s -> s
                   -> HTML  -- might become Javascript instead.
  showListAsTable :: Maybe (TSDLegend s) -> SharedPrecomputation s -> [s]
                   -> HTML
  showListAsTable _ pc i = foldMap (rowEnv . showAsTable Nothing pc) i
   where rowEnv = case tableLevel . Just $ head i of
          TabListItem -> id
          TabAtomic   -> H.td
          TabCols     -> H.tr
          TabRows     -> H.td . H.table
  tableLevel :: Functor p => p s -> TableLevel
  tableLevel _ = TabAtomic

instance TabShow Int where
  precompute _ = ()
  showAsTable _ _ i = H.toHtml i
instance TabShow Integer where
  precompute _ = ()
  showAsTable _ _ i = H.toHtml i
instance TabShow Char where
  type TSDLegend Char = ()
  precompute _ = ()
  showAsTable _ _ i = H.toHtml i
  showListAsTable _ _ i = H.toHtml i
  tableLevel _ = TabListItem
instance (TabShow s, TabShow (TSDLegend s), Monoid (SharedPrecomputation s))
           => TabShow [s] where
  type TSDLegend [s] = TSDLegend s
  type SharedPrecomputation [s] = SharedPrecomputation s
  precompute = foldMap precompute
  showAsTable = showListAsTable
  tableLevel pq = case tableLevel $ fmap head pq of
          TabListItem -> TabAtomic
          TabAtomic   -> TabCols
          TabCols     -> TabRows
          TabRows     -> TabCols

instance (Show s, TabShow s) => IHaskellDisplay (Table s) where
  display (Table i l)
      = display . H.table $ showAsTable l (precompute i) i
   where tableEnv = case tableLevel $ Just i of
          TabAtomic -> id
          _         -> H.table
  


