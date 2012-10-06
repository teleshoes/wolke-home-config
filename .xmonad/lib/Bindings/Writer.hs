{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Bindings.Writer
    ( module Control.Monad.Writer
    , Bindings(..)
    , (@@), (#)
    , bwBindList, bwFindOverlap
    , prettyBindings
    ) where

import XMonad
import Control.Arrow
import Control.Monad.Writer
import Data.Function
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Ord

import Bindings.Keys

data Bindings a
    = Binds { bDesc  :: String
            , bBinds :: [a] }
    | BList { bDesc  :: String
            , bList  :: [Bindings a] }
    deriving (Show)

type BW a = Writer (Bindings a) ()
type LW a = Writer [a] ()

flattenBW :: BW a -> [(Int, (String, Maybe [a]))]
flattenBW = go 0 . execWriter
  where
    go n Binds{..} = [(n, (bDesc, Just bBinds))]
    go n BList{..} = [(n, (bDesc, Nothing))] ++ concatMap (go $ n+1) bList

mapFlatBW :: (Int -> String -> Maybe [a] -> b) -> BW a -> [b]
mapFlatBW f = map (uncurry $ uncurry . f) . flattenBW

bwBindList :: BW a -> [a]
bwBindList = concat . catMaybes .  map (snd . snd) . flattenBW

bwFindOverlap :: Ord a => BW (a, b) -> [[(String, a)]]
bwFindOverlap = filter ((/= 1) . length)
              . groupBy ((==) `on` snd) . sortBy (comparing snd)
              . map (second fst) . bwDescList

bwDescList :: BW a -> [(String, a)]
bwDescList = go . execWriter
  where
    go Binds{..} = map (bDesc, ) bBinds
    go BList{..} = map (first $ \s -> bDesc ++ ":" ++ s) $ concatMap go bList

prettyBindings :: Show (PrettyBind a) => BW ((KeyMask, a), b) -> String
prettyBindings bw = concat . mapFlatBW pLine $ bw
  where
    l = maximum $ mapFlatBW (\n s _ -> 4*n + length s + 2) bw
    ind n = replicate (4*n) ' '
    pLine n s Nothing   = ind n ++ s ++ "\n"
    pLine n s (Just as) = pad l (ind n ++ s ++ ":")
                        ++ show (map (PBind . fst) as) ++ "\n"

infixr 0 @@, #

(#) :: a -> b -> LW (a, b)
a # b = tell [(a, b)]

class TellBs a b | a -> b where
    (@@) :: String -> a -> BW b

instance TellBs (BW a) a where
    s @@ bw = tell . BList s $ case bs of BList "" l -> l ; _ -> [bs]
      where bs = execWriter bw

atat   f s b = tell $ Binds s (f b)
atatL  f = atat $ concatMap f
atatLW f = atat $ concatMap f . execWriter

dist = uncurry zip . second repeat

type Key = (KeyMask, KeySym)
type KeyAct = X ()
type KeyBind = (Key, KeyAct)
type KeysBind = ([Key], KeyAct)

instance TellBs KeyBind         KeyBind where (@@) = atat   return
instance TellBs [KeyBind]       KeyBind where (@@) = atatL  return
instance TellBs (LW KeyBind)    KeyBind where (@@) = atatLW return

instance TellBs KeysBind        KeyBind where (@@) = atat   dist
instance TellBs [KeysBind]      KeyBind where (@@) = atatL  dist
instance TellBs (LW KeysBind)   KeyBind where (@@) = atatLW dist

type Mse = (KeyMask, Button)
type MseAct = Window -> X ()
type MseBind = (Mse, MseAct)
type MsesBind = ([Mse], MseAct)

instance TellBs MseBind         MseBind where (@@) = atat   return
instance TellBs [MseBind]       MseBind where (@@) = atatL  return
instance TellBs (LW MseBind)    MseBind where (@@) = atatLW return

instance TellBs MsesBind        MseBind where (@@) = atat   dist
instance TellBs [MsesBind]      MseBind where (@@) = atatL  dist
instance TellBs (LW MsesBind)   MseBind where (@@) = atatLW dist

instance TellBs ([a], [b])      (a, b)  where (@@) = atat   $ uncurry zip
instance TellBs [([a], [b])]    (a, b)  where (@@) = atatL  $ uncurry zip
instance TellBs (LW ([a], [b])) (a, b)  where (@@) = atatLW $ uncurry zip

instance TellBs (LW a) a => TellBs [LW a] a where
    s @@ a = s @@ sequence_ a

-- Bindings Instances
instance Monoid (Bindings a) where
    mempty = BList "" []

    mappend (BList "" ls) (BList "" rs) = BList "" (ls ++ rs)
    mappend (BList "" ls) r             = BList "" (ls ++ [r])
    mappend l             (BList "" rs) = BList "" (l : rs)
    mappend l             r             = BList "" [l,r]

instance Functor Bindings where
    fmap f (Binds s bs) = Binds s $ fmap f bs
    fmap f (BList s bs) = BList s $ fmap (fmap f) bs

instance Show (PrettyBind a) => Show (BW ((KeyMask, a), b)) where
    show = prettyBindings
