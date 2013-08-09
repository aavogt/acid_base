{-# LANGUAGE TupleSections #-}
module Main.PpKaEq
    ( ppKaEq
    ) where
import AcidBase.Internal (species)
import AcidBase.Solve
import qualified Data.Foldable as F (toList)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, mapMaybe)
import Text.Printf (printf)
import Text.Trifecta hiding (string)
import qualified Text.XHtml as X ((<<), td, th)

import Control.Lens

ppKaEq problem solution =
    let (cK, cE) = unzip $ M.toList missing ++ sorted
         where
            s' = solution ^. solution_concentrations
            sorted = mapMaybe (\x -> fmap (x,) $ x `M.lookup` s')
                        . concatMap F.toList
                        . species
                        . view problem_equilibrium $ problem
            missing = s' M.\\ M.fromList sorted
        (sK, sE) = solution ^. solution_supplementary . to (unzip . M.toList)
        iterationNumber = show (solution ^. solution_iteration)

    in let
        rowNames = map (X.th X.<<) $ "iteration" : cK ++ sK
        rowContent = map (X.td X.<<)
                $ iterationNumber : map (printf "%.3g") (cE ++ sE)
    in (rowNames, rowContent)
