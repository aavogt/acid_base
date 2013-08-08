{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
module AcidBase.Internal.Rejoins (rejoins
) where

import qualified Data.Map as M (delete, fromList, insert, lookup, toList)
import Data.Maybe (mapMaybe)
import qualified Data.Sequence as S ((<|), (><), Seq, viewl, ViewL((:<)), viewr, ViewR((:>)))

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
