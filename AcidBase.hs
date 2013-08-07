{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
module AcidBase (kaEqs, kAs, charge, species) where

import Data.Packed.Matrix
import Data.Packed.Vector
import Numeric.LinearAlgebra.LAPACK
import Numeric.GSL.Root
import qualified Data.Map as M
import Data.List
import Data.Maybe
import Control.Applicative
import qualified Data.Sequence as S
import qualified Data.Foldable as F
import Data.Monoid
import Control.Monad



kAs :: M.Map (String, String) Double
kAs = M.fromList $ [ (("PO4","HPO4"), 2.14e-13), (("HPO4","H2PO4"), 6.2e-8), (("H2PO4","H3PO4"), 7.5e-3),
    (("Ac", "HAc"), 10**(-4.76))]


charge :: M.Map String Int
charge = M.fromList [ ("PO4", -3) , ("HPO4", -2), ("H2PO4", -1), ("H3PO4", 0),
    ("Ac", -1), ("HAc", 0)]

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

collect' :: Ord a => M.Map a a -> [S.Seq a]
collect' = unfoldr step
    where
    step m
        | M.size m == 0 = Nothing
        | (e0, _ ) <- M.elemAt 0 m,
          s <- e0 S.<| S.unfoldr (\e -> dup <$> M.lookup e m) e0,
          m <- M.delete e0 m = Just (s, m)
    dup x = (x,x)


species = rejoins . collect' . M.fromList . M.keys

-- kaEqs :: M.Map String Double -> M.Map (String,String) Double -> Matrix Double

kaEqs charge kAs input = unfoldr (\i -> do
        let (r,i') = kaEqs' charge kAs input i
        when (i/=0) $ do
            -- guard (abs(i - i')/1e-3 > 1) -- abs tolerance
            guard ( abs((i-i') / i') > 1e-5 ) -- relative
        return (r,i') ) 0

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


-- http://www.ocean.washington.edu/courses/oc400/Lecture_Notes/CHPT6.pdf
davies i z1 z2 = exp( - 0.5 * fromIntegral z1 * fromIntegral z2 * ( sqrt i / (1+sqrt i) - 0.2 * i ))

-- also check about multiple sequences?
test = ssn' "a" 5 == ssn "a" 5
 where
   ssn' a nn = [S.fromList [ a ++ show n | n <- [1 .. (nn+1) :: Double]]]

   ssn a nn = species $ M.fromList [ ((a ++ show n, a ++ show (n+1)), n) | n <- [1 .. nn :: Double] ]
