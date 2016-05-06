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

module IHaskell.Tables.Data (Table, tabular, tableWithLegend, TabShow) where


import IHaskell.Display
import IHaskell.Display.Blaze

import qualified Text.Blaze.Html4.Strict as H
import qualified Text.Blaze.Html4.Strict.Attributes as HA

import Data.Monoid

-- | A tabular view of Haskell data.
data Table d = Table { getTableContent :: d
                     , tableLegend :: Maybe (TSDLegend d)
                     }

tabular :: d -> Table d
tabular x = Table x Nothing

tableWithLegend :: TabShow d => TSDLegend d -> d -> Table d
tableWithLegend legend x = Table x $ pure legend

type HTML = H.Html

data TableLevel = TabListItem | TabAtomic
                | TabTupCols  | TabTupRows
                | TabListCols | TabListRows

class (Monoid (SharedPrecomputation s)) => TabShow s where
  type TSDLegend s :: *
  type TSDLegend s = String
  type SharedPrecomputation s :: *
  type SharedPrecomputation s = ()
  
  precompute :: s -> SharedPrecomputation s
  precompute _ = mempty
  
  showAsTable :: Maybe (TSDLegend s) -> SharedPrecomputation s -> s
                   -> HTML  -- might become Javascript instead.
  
  showListAsTable :: (TabShow (TSDLegend s), Monoid (SharedPrecomputation (TSDLegend s)))
            => Maybe (TSDLegend s) -> SharedPrecomputation s -> [s] -> HTML
  showListAsTable l pc i = addLegend l $ foldMap (rowEnv . showAsTable Nothing pc) i
   where rowEnv = case tableLevel . Just $ head i of
          TabListItem -> id
          TabAtomic   -> H.td
          TabTupCols  -> H.tr
          TabTupRows  -> H.td . H.table
          TabListCols -> H.tr
          TabListRows -> H.td . H.table
         addLegend Nothing t = t
         addLegend (Just l) t = H.thead (rowEnv $ showAsTable Nothing mempty l)
                             <> H.tbody t
  tableLevel :: Functor p => p s -> TableLevel
  tableLevel _ = TabAtomic

instance TabShow () where
  type TSDLegend () = ()
  showAsTable _ _ () = mempty
  showListAsTable _ _ _ = mempty
  tableLevel _ = TabListItem
instance TabShow Int where
  showAsTable _ _ i = H.toHtml i
instance TabShow Integer where
  showAsTable _ _ i = H.toHtml i
instance TabShow Char where
  showAsTable _ _ i = H.toHtml i
  showListAsTable _ _ i = H.toHtml i
  tableLevel _ = TabListItem
instance ( TabShow s, TabShow (TSDLegend s) )
           => TabShow [s] where
  type TSDLegend [s] = TSDLegend s
  type SharedPrecomputation [s] = SharedPrecomputation s
  precompute = foldMap precompute
  showAsTable = showListAsTable
  tableLevel pq = case tableLevel $ fmap head pq of
          TabListItem -> TabAtomic
          TabAtomic   -> TabListCols
          TabTupCols  -> TabListRows
          TabTupRows  -> TabListCols
          TabListCols -> TabListRows
          TabListRows -> TabListCols
  
instance (TabShow s, TabShow t) => TabShow (s,t) where
  type TSDLegend (s,t) = (TSDLegend s, TSDLegend t)
  type SharedPrecomputation (s,t) = (SharedPrecomputation s, SharedPrecomputation t)
  precompute (x,y) = (precompute x, precompute y)
  showAsTable l (px,py) (x,y) = fst colEnv (showAsTable (fmap fst l) px x)
                             <> snd colEnv (showAsTable (fmap snd l) py y)
   where colEnv = case tableLevel $ Just x of
          TabTupCols  -> (id, case tableLevel $ Just y of
                                         TabListItem -> H.td
                                         TabAtomic   -> H.td
                                         TabListCols -> id
                                         TabTupCols  -> id
                                         _           -> H.td . H.table )
          TabTupRows  -> (id, case tableLevel $ Just y of
                                         TabListRows -> id
                                         TabTupRows  -> id
                                         _           -> H.tr )
          TabListCols -> (H.tr, H.tr . case tableLevel $ Just y of
                                         TabListCols -> id
                                         TabTupCols  -> id
                                         _           -> H.table )
          TabListRows -> (H.td . H.table, H.td . H.table)
          _           -> (H.td, case tableLevel $ Just y of
                                         TabListItem -> H.td
                                         TabAtomic   -> H.td
                                         TabListCols -> id
                                         TabTupCols  -> id
                                         _           -> H.td . H.table )

  tableLevel pq = case tableLevel $ fmap fst pq of
          TabListItem -> TabTupCols
          TabAtomic   -> TabTupCols
          TabTupCols  -> TabTupCols
          TabTupRows  -> TabTupRows
          TabListCols -> TabTupRows
          TabListRows -> TabTupCols

instance (Show s, TabShow s) => IHaskellDisplay (Table s) where
  display (Table i l)
      = display . H.table $ showAsTable l (precompute i) i
   where tableEnv = case tableLevel $ Just i of
          TabAtomic -> id
          _         -> H.table
  


