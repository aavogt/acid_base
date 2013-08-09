{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
module AcidBase.Internal (collect'
,rejoins
,species
) where



import Control.Applicative ((<$>))
import Data.List (unfoldr)
import qualified Data.Map as M (delete, elemAt, fromList, insert, keys, lookup, Map, size, toList)
import Data.Maybe (mapMaybe)
import qualified Data.Sequence as S ((<|), (><), Seq, unfoldr, viewl, ViewL((:<)), viewr, ViewR((:>)))


collect' :: Ord a => M.Map a a -> [S.Seq a]
collect' = unfoldr step
    where
    step m
        | M.size m == 0 = Nothing
        | (e0, _ ) <- M.elemAt 0 m,
          s <- e0 S.<| S.unfoldr (\e -> dup <$> M.lookup e m) e0,
          m <- M.delete e0 m = Just (s, m)
    dup x = (x,x)


-- | connects the sequences whose ends are the same element.
-- For example:
--
--  >>> rejoins [ [a,b,c], [c,d], [d,e] ]
--
--  > [ [a,b,c,d,e] ]
rejoins :: Ord a => [S.Seq a] -> [S.Seq a]
rejoins xs = g ls0 xs
    where
    ls0 = M.fromList $ mapMaybe (\x -> case S.viewl x of
            x S.:< xs -> Just (x, xs)
            _ -> Nothing) xs

    g ls (x:xs)
        | rs S.:> r <- S.viewr x,
          Just left <- M.lookup r ls,
          ls <- M.delete r ls = g ls ( (x S.>< left) : xs)
        | a S.:< as <- S.viewl x = g (M.insert a as ls) xs
        | otherwise = g ls xs
    g ls _ = map (\(x,xs) -> x S.<| xs) $ M.toList ls



species = rejoins . collect' . M.fromList . M.keys

