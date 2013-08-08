{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
module AcidBase.Internal.KaEqs (kaEqs'
    , kaEqs''
) where

import AcidBase.Internal.Davies (davies)
import AcidBase.Species (species)
import Control.Applicative ((<$>))
import qualified Data.Foldable as F (any, sum, toList)
import qualified Data.Map as M ((!), fromList, fromListWith, insert, lookup, map, Map, mapWithKey, member, singleton, size, toList)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Monoid ((<>))
import Data.Packed.Matrix (asColumn, flatten, fromRows)
import Data.Packed.Vector ((@>), fromList)
import qualified Data.Sequence as S (index, length)
import Numeric.GSL.Root (uniRoot, UniRootMethod(Bisection))
import Numeric.LinearAlgebra.LAPACK (linearSolveR)

kaEqs' charge kAs input i = snd $ f (fst sol)
    where f = kaEqs'' charge kAs input i . (10**) . negate
          sol = uniRoot Bisection 1e-3 20 (fst . f) (-2) 20

kaEqs'' charge kAs input i h = (en, ((solnMap, (davies i 1 (-1), i)), i2))
   where
    en = (F.sum $ M.mapWithKey (\s i -> maybe 0 (\c-> fromIntegral c*(soln @> i)) (M.lookup s charge)) sm)
            + h - spectatorCharges


    i2 = ((F.sum $ M.mapWithKey (\s i -> maybe 0 (\c-> (fromIntegral c)^2 *(soln @> i))
                    (M.lookup s charge)) sm)
            + h + spectatorI) / 2
    spectatorI = F.sum $ M.mapWithKey (\s c -> maybe 0 ((c*) . (^2) . fromIntegral) $ M.lookup s charge) input

    spectatorCharges = F.sum $ M.mapWithKey (\s c -> maybe 0 ((c*) . fromIntegral) $ M.lookup s charge) input

    matrix = fromRows ( mbalLhs ++ keqLhs )
    rhs = fromList $ mbalRhs <> keqRhs

    soln = flatten $ linearSolveR matrix (asColumn rhs)

    solnMap = M.insert "H" h $ M.map (soln @>) sm

    validSpecies = filter (F.any (`M.member` input)) $ species kAs

    sm :: M.Map String Int
    sm = M.fromList $ zipWith (,) (concat $ map F.toList validSpecies) [0 .. ]
    name s | Just x <- M.lookup s sm = x
        | otherwise = error s

    toDenseBool f = fromList [ if f n then 1 else 0  | n <- [0 .. M.size sm - 1] ]
    toDense f = fromList [ f n  | n <- [0 .. M.size sm - 1] ]

    mbalRhs = map (sum . mapMaybe (`M.lookup` input) . F.toList) validSpecies
    mbalLhs = toDenseBool . f <$> validSpecies
        where f x = let l = sm M.! (x `S.index` 0)
                        u = l + S.length x
                in \n -> n >= l && n < u


    kAs2 = M.fromListWith (<>) $ map (\((k1,k2),v) -> (k1,M.singleton k2 v)) $ M.toList $ kAs

    keqRhs = replicate (length keqLhs) 0

    keqLhs = mapMaybe getRow (concat $ map F.toList validSpecies)
        where getRow s = do
                    [(t, n)] <- M.toList <$> M.lookup s kAs2
                    let gammaPM =
                                let z1 = fromMaybe 0 $ M.lookup s charge
                                    z2 = fromMaybe 0 $ M.lookup t charge
                                in davies i z1 z1 * davies i 1 1 / davies i z2 z2
                    return $ toDense $ \i -> fromMaybe 0 $ lookup i [ (name s, -h * gammaPM),  (name t,n) ]
