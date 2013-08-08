{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
module AcidBase.Internal.Collect (collect'
) where

import Control.Applicative ((<$>))
import Data.List (unfoldr)
import qualified Data.Map as M (delete, elemAt, lookup, Map, size)
import qualified Data.Sequence as S ((<|), Seq, unfoldr)

collect' :: Ord a => M.Map a a -> [S.Seq a]
collect' = unfoldr step
    where
    step m
        | M.size m == 0 = Nothing
        | (e0, _ ) <- M.elemAt 0 m,
          s <- e0 S.<| S.unfoldr (\e -> dup <$> M.lookup e m) e0,
          m <- M.delete e0 m = Just (s, m)
    dup x = (x,x)
