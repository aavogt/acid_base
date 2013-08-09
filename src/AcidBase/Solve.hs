{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
module AcidBase.Solve where

import AcidBase.Internal (species)
import AcidBase.Internal.Davies (davies)
import Control.Applicative ((<$>))
import Control.Monad (guard, when)
import qualified Data.Foldable as F (any, sum, toList)
import Data.List (unfoldr)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Monoid ((<>))
import Data.Packed.Matrix (asColumn, flatten, fromRows)
import Data.Packed.Vector ((@>), fromList)
import qualified Data.Sequence as S (index, length)
import Numeric.GSL.Root (uniRoot, UniRootMethod(Bisection))
import Numeric.LinearAlgebra.LAPACK (linearSolveR)

import Control.Lens

import Data.Default

import AcidBase.Data
import Control.Monad

data Activity k c
    = ActivityFn { _getActivityFn :: Problem k c -> k -> c }
    | Electrostatic
        { _electrostatic :: Electrostatic,
          _ionic_strength_atol, _ionic_strength_rtol :: Maybe c,
          _ionic_strength :: c,
          _electrostatic_maxit :: Int }

data Electrostatic = Davies | DebyeHuckel
    deriving (Show)

data Problem k c = Problem {
    _problem_ionCharge :: M.Map k Int,
    _problem_equilibrium :: M.Map (k,k) c,
    _problem_initialConcentration :: M.Map k c,
    _problem_activityModel :: Activity k c,
    _problem_pH_bounds :: (c,c),
    _problem_ideal_maxit :: Int,
    _problem_ideal_atol :: c }

instance Default (Problem String Double) where
    def = defaultProblem

defaultProblem = Problem {
    _problem_ionCharge = charge,
    _problem_equilibrium = kAs,
    _problem_initialConcentration = M.fromList [("H2O", 55), ("H2PO4", 1.15e-3), ("HPO4", 8.06e-3)],
    _problem_activityModel = Electrostatic { _electrostatic = Davies,
                            _ionic_strength = 0,
                            _ionic_strength_atol = Nothing,
                            _ionic_strength_rtol = Just 1e-5,
                            _electrostatic_maxit = 5 },
    _problem_pH_bounds = (-2, 20),
    _problem_ideal_maxit = 100,
    _problem_ideal_atol = 1e-3 }

data Solution k c = Solution
    { _solution_concentrations :: M.Map k c,
      _solution_supplementary :: M.Map String c,
      _solution_iteration :: [Int] }

fmap concat $ mapM makeLenses [''Activity, ''Problem, ''Solution ]

solve :: Problem String Double -> [Solution String Double]
solve p | e <- view problem_activityModel p =
    take (p ^? problem_activityModel . electrostatic_maxit & fromMaybe 1) $
    flip unfoldr (1, p) $ \(iteration, p) -> do
            let r = solve1 p
                Just i' = r ^?! solution_supplementary . at "ionic strength"
                i = p ^?! problem_activityModel . ionic_strength
            when (i /= 0) $ do
                guard $ e ^?! ionic_strength_atol . to (Just (abs(i - i')) >) -- abs tolerance
                guard $ e ^?! ionic_strength_rtol . to (Just (abs((i - i')/i')) >) -- abs tolerance
            return (r & solution_iteration %~ (iteration:),
                    (iteration+1,
                     p & problem_activityModel . ionic_strength .~ i'))


deltaEnKey ="electroneutrality error"

solve1 p = f (fst sol)
    where f = solve1h p . (10**) . negate
          getDEN = views solution_supplementary (M.! deltaEnKey)
          sol = uniRoot
            Bisection
            (p ^. problem_ideal_atol)
            (p ^. problem_ideal_maxit)
            (getDEN . f)
            `uncurry` view problem_pH_bounds p


solve1h
    p @ Problem { _problem_ionCharge = charge, _problem_equilibrium = kAs,
              _problem_initialConcentration = input } h =
  let
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
                                let f = unActivityFn (view problem_activityModel p) p
                                in f s * f "H" / f t
                    return $ toDense $ \i -> fromMaybe 0 $ lookup i [ (name s, -h * gammaPM),  (name t,n) ]
  in
        Solution { _solution_concentrations = solnMap,
                   _solution_supplementary = M.fromList [
                    ("ionic strength", i2),
                    ("gamma11", unActivityFn (view problem_activityModel p) p "H"),
                    (deltaEnKey, en)
                   ],
                   _solution_iteration = [1] }

unActivityFn
    Electrostatic { _electrostatic = Davies, _ionic_strength = i } = \p k ->
          let z | "H" <- k = 1
                | otherwise = fromMaybe 0 $ M.lookup k (_problem_ionCharge p)
          in davies i z z
unActivityFn (ActivityFn f) = f
