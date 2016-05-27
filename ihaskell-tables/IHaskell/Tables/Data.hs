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
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UnicodeSyntax        #-}

module IHaskell.Tables.Data ( Table, tabular, tableWithLegend, TabShow
                            , approxAndTooltip ) where


import IHaskell.Display
import IHaskell.Display.Blaze

import Text.Blaze.Html ((!))
import qualified Text.Blaze.Html4.Strict as H
import qualified Text.Blaze.Html4.Strict.Attributes as HA
import qualified Text.Blaze.Html.Renderer.Utf8 as HR

import Stitch as Stitch
import Stitch.Combinators as Stitch
import Stitch.Render as Stitch
import Control.Monad.Stitch as Stitch

import Data.Semigroup
import Data.Foldable
import Data.Function ((&))
import Data.Map (Map)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map as Map
import Data.Char (isSpace)
import Data.String (IsString(..))
import qualified Data.Text as Txt
import Numeric (showGFloat)
import Control.Applicative
import Control.Monad (forM_)
import Control.Arrow

import Data.List.Stretchable (Stretch)

-- | A tabular view of Haskell data.
data Table d = Table { getTableContent :: d
                     , tableLegend :: Maybe (TSDLegend d)
                     , tableStyle :: CSS
                     }




globalDefStyle :: CSS
globalDefStyle = do
   "table"? do
      forM_ [id, ("td"?), ("tr"?)]
             ($ "border-style" .= "none")
      "td"? "padding-left" .= "1.5em"
      "thead"? "border-bottom" -: do
          "style" .= "solid"
   ".approxWithTooltip"? do
     ".exactShowTooltip"? do
         "visibility" .= "hidden"
         "position" .= "absolute"
         "background-color" .= "#ddd"
         "color" .= "#111"
         "border" .= "1px solid #bbb"
         "border-radius" .= "3px"
         "z-index" .= "1"
   ".approxWithTooltip:hover"?
     ".exactShowTooltip"? do
         "visibility" .= "visible !important"

tabular :: d -> Table d
tabular x = Table x Nothing globalDefStyle

tableWithLegend :: TabShow d => TSDLegend d -> d -> Table d
tableWithLegend legend x = Table x (pure legend) globalDefStyle

type HTML = H.Html

asHTMLClass :: String -> H.Attribute
asHTMLClass = HA.class_ . fromString

type HTMLClass = String

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
  
  showAsPlaintext :: SharedPrecomputation s -> s -> String
  
  showAsPlaintextAndHtml :: SharedPrecomputation s -> s -> (String, HTML)
  showAsPlaintextAndHtml pc i = (ptxt, fromString ptxt)
   where ptxt = showAsPlaintext pc i
  
  showAsTable :: Maybe (TSDLegend s) -> SharedPrecomputation s -> s
                   -> TableView
  showAsTable _ pc i = TableView [pure <$> fromString ptxt] True htm
   where (ptxt, htm) = showAsPlaintextAndHtml pc i
  
  showListAsTable :: (TabShow (TSDLegend s), Monoid (SharedPrecomputation (TSDLegend s)))
            => Maybe (TSDLegend s) -> SharedPrecomputation s -> [s] -> TableView
  showListAsTable l pc i = addLegend l $ foldMap (rowEnv . showAsTable Nothing pc) i
   where rowEnv (TableView tx tp md) = txtAlignDir $ TableView tx tp (htmAlign md)
           where (htmAlign, txtAlignDir) = case tableLevel . Just $ head i of
                  TabListItem -> (id,           horizontalCompos)
                  TabAtomic   -> (td,           horizontalCompos)
                  TabTupCols  -> (tr,           verticalCompos  )
                  TabTupRows  -> (td . H.table, horizontalCompos)
                  TabListCols -> (tr,           verticalCompos  )
                  TabListRows -> (td . H.table, horizontalCompos)
         addLegend Nothing t = t
         addLegend (Just l) (TableView tx tt md)
               = rowEnv (showAsTable Nothing mempty l)
              <> TableView tx      False (H.tbody md)
         [td,tr] = fmap (!asHTMLClass (tableCellClass . Just $ head i)) <$> [H.td, H.tr]
  
  tableLevel :: Functor p => p s -> TableLevel
  tableLevel _ = TabAtomic
  
  tableCellClass :: Functor p => p s -> HTMLClass
  tableCellClass _ = "CustomData"
  
  defaultStyling, defaultTdStyle, defaultTrStyle :: Functor p => p s -> CSS
  defaultStyling d = do
        fromString ("td."<>tableCellClass d)? defaultTdStyle d
        fromString ("tr."<>tableCellClass d)? defaultTrStyle d
  defaultTdStyle _ = mempty
  defaultTrStyle _ = mempty

instance TabShow () where
  type TSDLegend () = ()
  showAsTable _ _ () = mempty
  showListAsTable _ _ _ = mempty
  tableLevel _ = TabListItem
  tableCellClass _ = "Unit"

instance TabShow Int where
  showAsPlaintext _ = show
  tableCellClass _ = "Int"
  defaultTdStyle _ = "text-align" .= "right"

instance TabShow Integer where
  showAsPlaintext _ = show
  tableCellClass _ = "Integer"
  defaultTdStyle _ = "text-align" .= "right"

instance TabShow Char where
  showAsPlaintext _ = show
  showListAsTable _ _ = fromString
  tableLevel _ = TabListItem
  tableCellClass _ = "Char"

instance TabShow Double where
  type SharedPrecomputation Double = FloatShowReps Double
  precompute = singletonFSR
  showAsTable _ lookSRep n = uncurry approxAndTooltip
                              (stringReps fsr)
                              (avoidGaps . toHtmlWithEnSpaces
                                 $ takeWhile isSpace (head fsr) ++ show n)
   where Just fsr = lookupFSR lookSRep n
         stringReps qs | (exact:_) <- filter ((~=n) . read) $ take 3 qs
               = mantissaMod exact toHtmlWithEnSpaces
         stringReps (_:_:_:q₃:_) = mantissaMod q₃ $ reverse >>> \case
                   mω:mψ:ms -> toHtmlWithEnSpaces (reverse ms)
                     `mappend` withStyle ("opacity".="0.42") (H.span $ H.toHtml mψ)
                     `mappend` withStyle ("opacity".="0.12") (H.span $ H.toHtml mω)
         mantissaMod q f = (,q) . avoidGaps $ f mantis `mappend` case expon of
                 []       -> mempty
                 ('e':ex) -> H.preEscapedText "&middot;" `mappend` H.sub "10"
                                   `mappend` H.sup (H.toHtml ex)
          where (mantis,expon) = break (=='e') q
         a ~= b   -- virtually-equal (up to rounding of small rationals in floating-point)
             = (a/=a && b/=b) -- both NaN
               || a==b
               || abs (b-a) <= ε*(abs a+abs b)
         ε = 2e-14
  tableCellClass _ = "Double"
  defaultTdStyle _ = "text-align" .= "left"

instance TabShow Float where
  type SharedPrecomputation Float = FloatShowReps Double
  precompute n = precompute (realToFrac n :: Double)
  showAsTable l pc n = showAsTable l pc (realToFrac n :: Double)

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
  tableCellClass = ("List-"<>) . tableCellClass . fmap head
  defaultStyling = defaultStyling . fmap head
  defaultTdStyle _ = mempty
  defaultTrStyle _ = mempty
  
instance (TabShow s, TabShow t) => TabShow (s,t) where
  type TSDLegend (s,t) = (TSDLegend s, TSDLegend t)
  type SharedPrecomputation (s,t) = (SharedPrecomputation s, SharedPrecomputation t)
  precompute (x,y) = (precompute x, precompute y)
  showAsTable l (px,py) (x,y)
           = TableView (ptx++pty) False
                              $ fst colEnv htmx `mappend` snd colEnv htmy
   where TableView ptx False htmx = verticalCompos $ showAsTable (fmap fst l) px x
         TableView pty False htmy = verticalCompos $ showAsTable (fmap snd l) py y
         colEnv = case tableLevel $ Just x of
          TabTupCols  -> (id, case tableLevel $ Just y of
                                         TabListItem -> tdy
                                         TabAtomic   -> tdy
                                         TabListCols -> id
                                         TabTupCols  -> id
                                         _           -> tdy . H.table )
          TabTupRows  -> (id, case tableLevel $ Just y of
                                         TabListRows -> id
                                         TabTupRows  -> id
                                         _           -> try )
          TabListCols -> (trx, try . case tableLevel $ Just y of
                                         TabListCols -> id
                                         TabTupCols  -> id
                                         _           -> H.table )
          TabListRows -> (tdx . H.table, tdy . H.table)
          _           -> (tdx, case tableLevel $ Just y of
                                         TabListItem -> tdy
                                         TabAtomic   -> tdy
                                         TabListCols -> id
                                         TabTupCols  -> id
                                         _           -> tdy . H.table )
         [tdx,trx] = fmap (!asHTMLClass (tableCellClass $ Just x)) <$> [H.td, H.tr]
         [tdy,try] = fmap (!asHTMLClass (tableCellClass $ Just y)) <$> [H.td, H.tr]

  tableLevel pq = case tableLevel $ fmap fst pq of
          TabListItem -> TabTupCols
          TabAtomic   -> TabTupCols
          TabTupCols  -> TabTupCols
          TabTupRows  -> TabTupRows
          TabListCols -> TabTupRows
          TabListRows -> TabTupCols
  tableCellClass pq = tableCellClass (fmap fst pq) <> "-" <> tableCellClass (fmap snd pq)
  defaultStyling pq = defaultStyling (fmap fst pq) `mappend` defaultStyling (fmap snd pq)
  defaultTdStyle _ = mempty
  defaultTrStyle _ = mempty

instance (Show s, TabShow s) => IHaskellDisplay (Table s) where
  display (Table i l globalSty)
      = display $
          H.div (
               css2html (".IHaskell-table"? globalSty`mappend`stylings) `mappend`
               H.table htm ! asHTMLClass (tableCellClass $ Just i)
             ) ! HA.class_ "IHaskell-table"
   where TableView ptx False htm = showAsTable l (precompute i) i
         tableEnv = case tableLevel $ Just i of
          TabAtomic -> id
          _         -> H.table
         stylings = defaultStyling $ Just i
  


css2html :: CSS -> HTML
css2html = {-! HA.scoped True -} H.style . H.preEscapedText . renderCSS

withStyle :: CSS -> HTML -> HTML
withStyle s h = h ! HA.style (H.toValue sren)
 where sren = case runStitch $ "?"? s of
         ((), blck) -> case Txt.unpack $ Stitch.compressed blck of
             ('?':'{':blcc) -> takeWhile (/='}') blcc

avoidGaps :: HTML -> HTML
avoidGaps = H.unsafeLazyByteString . HR.renderHtml

data FloatShowReps f
    = FloatShowReps { negativeShowReps, positiveShowReps :: Map (NaNSafe f) [String] }

-- | Make IEEE-754 fully-ordered: @-∞ < -1 < 0 < 1 < ∞ < NaN@.
--   Only this way are floats actually safe as `Map` keys.
newtype NaNSafe f = NaNSafe { getNaNSafeNum :: f }

instance (Eq f) => Eq (NaNSafe f) where
  NaNSafe a == NaNSafe b = a/=a && b/=b || a==b
instance (Ord f) => Ord (NaNSafe f) where
  compare (NaNSafe a) (NaNSafe b)
    | a/=a, b==b     = GT
    | b/=b, a==a     = LT
    | a/=a, b/=b     = EQ
    | otherwise      = compare a b

singletonFSR :: Double -> FloatShowReps Double
singletonFSR x | x<0        = FloatShowReps fsrMap mempty
               | otherwise  = FloatShowReps mempty fsrMap
 where fsrMap = Map.singleton (NaNSafe n)
                              [showGFloat (Just preci) n "" | preci<-[0..]]
       n = abs x

lookupFSR :: (Num f, Ord f) => FloatShowReps f -> f -> Maybe [String]
lookupFSR (FloatShowReps negReps posReps) n
   | n<0        = map negative <$> Map.lookup (NaNSafe $ -n) negReps
   | otherwise  = map positive <$> Map.lookup (NaNSafe n) posReps
 where positive = (' ':)
       negative nr = spcs ++ '-':num
        where (spcs,num) = span (==' ') nr

instance ∀ f . (RealFloat f, Read f) => Monoid (FloatShowReps f) where
  mempty = FloatShowReps mempty mempty
  mappend (FloatShowReps f₁n f₁p) (FloatShowReps f₂n f₂p) = FloatShowReps
               (Map.fromList . ascIndent . rmAllFDups . Map.toList $ f₁n<>f₂n)
               (Map.fromList . ascIndent . rmAllFDups . Map.toList $ f₁p<>f₂p)
   where rmFalseDups _ [] = ([], False)
         rmFalseDups bck (o@(NaNSafe vh,(rh:rhs)) : vs)
                   = case takeWhile (conflicts . head . snd) =<< [vs,bck] of
             [] -> first (o:) $ rmFalseDups (o:bck) vs
             _  -> ((NaNSafe vh, rhs) : fst (rmFalseDups (o:bck) vs), True)
          where conflicts rc = abs (vh - read rc) <= δrd
                δrd = abs $ vh - read rh
         rmAllFDups l = case rmFalseDups [] l of
               (done, False) -> done
               (step, True)  -> rmAllFDups step
         
         ascIndent, descInd :: [(NaNSafe f, [String])] -> [(NaNSafe f, [String])]
         ascIndent = descInd . reverse
         descInd [] = []
         descInd (q₁@(_,(r₁:_)):qs) = q₁ : descInd (map (second $ map indMore) qs)
          where indMore r | r<r₁       = r
                          | otherwise  = indMore (' ':r)
 

toHtmlWithEnSpaces :: String -> HTML
toHtmlWithEnSpaces = foldMap ubs
 where ubs ' ' = H.preEscapedText "&ensp;"
       ubs c = H.toHtml c



-- | Use this if you want to display your type in two forms: one neat, concise but
--   inexact representation, and a more precise but also more clunky one.
approxAndTooltip
    :: HTML        -- ^ Short approximation / preview, for concise HTML tables.
    -> String      -- ^ Plaintext representation, should be /reasonably/ exact.
    -> HTML        -- ^ Exact representation, will only be shown on mouse-over in HTML.
    -> TableView   -- ^ Markup to use in 'showAsTable'.
approxAndTooltip approx plaintext tooltip
    = TableView [pure<$>fromString plaintext] True
              $ H.div ( ( H.span tooltip ! HA.class_ "exactShowTooltip"
                               & withStyle ( ("position".="absolute")
                                   `mappend` ("visibility".="hidden") ) )
                `mappend` approx
      ) ! HA.class_ "approxWithTooltip"




data TableView = TableView {
         plaintextTable :: [TextBlock]
       , isTransposed :: Bool
       , htmlTable :: HTML
       }
instance Semigroup TableView where
  TableView t₁ t₁t m₁<>TableView t₂ t₂t m₂
         = TableView (zipWith (<>) t₁ t₂') t₁t (mappend m₁ m₂)
   where t₂' | t₁t==t₂t   = t₂
             | otherwise  = map transposeTB t₂
instance Monoid TableView where
  mempty = TableView [pure""] True mempty
  mappend = (<>)
  
instance IsString TableView where
  

type TextBlock = Stretch (Stretch Char)

transposeTB :: TextBlock -> TextBlock
transposeTB = sequenceA

verticalCompos, horizontalCompos :: TableView -> TableView
verticalCompos (TableView t False m)
              = TableView t False m
verticalCompos (TableView t True m)
              = TableView (transposeTB<$>t) False m
horizontalCompos (TableView t True m)
                = TableView t True m
horizontalCompos (TableView [] _ m) = TableView [pure""] True m
horizontalCompos (TableView (t₀:ts) False m)
                = TableView (pure . sconcat $ transposeTB<$>(t₀:|ts)) True m
