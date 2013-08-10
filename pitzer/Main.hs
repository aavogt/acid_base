{-# LANGUAGE NoMonomorphismRestriction, ImplicitParams #-}
-- | calculations originally for analysis of current efficiencies in zinc
-- electrowinning (done in waterloo's CHE391). Should be able to adapt this.
module Main where
import Pz2
import Numeric.GSL.Root
import Control.Monad
import Text.Printf
import System.Environment
import NumVar

-- thermodynamic equilibrium constant for
--      H+ + HSO4- <=> so4=
lnk = (562.69486 - 102.5154 * log ?temp - 1.117033e-4 * ?temp ^ 2
    + 0.2477538 * ?temp - 13273.75/ ?temp) * log 10

objective znTot so4Tot [h, hso4, so4] =
    [ hso4 + so4 - so4Tot,              -- conserve S
      2* znTot + h - hso4  - 2*so4,     -- electoneutrality
      -- equilibrium
      - lnk + log h + log so4 - log hso4 +
         let ?c = CS h hso4 so4 znTot
         in lngH + lng2 - lng1
    ]

main = do
    [mode] <- getArgs
    case mode of
        "1" -> mainHByZnAct
        "2" -> mainContour

mainHByZnAct =
    writeFile "hByZnAct" $ unlines $
        "set zn h2so4 hByZnAct_m hByZnAct_M" :
        map (\x -> printf "A %f 100 %f %f" x
                        (actRatio False x 100) (actRatio True x 100))
            [20, 40, 60, 80]
        ++
        map (\x -> printf "B 60 %f %f %f" x
                        (actRatio False 60 x) (actRatio True 60 x))
            [60, 100, 120, 150]
        ++
        [printf "C 80 100 %f %f"
                (actRatio False 80 100) (actRatio True 80 100)]

mainContour = do
        -- [t,wtZn,wtH2SO4] <- mapM readIO =<< getArgs
    writeFile "acts" "wtZn wth2so4 ah a1 a2 azn gh g1 g2 gzn\n"
    forM_ [10, 15 .. 100] $ \wtZn ->
        forM_ [10, 15 .. 150] $ \wtH2SO4 ->
        appendFile "acts"
            $ ($ "\n")
            $ foldr (\x y -> x . (' ':) . y) id $ map shows $ [wtZn, wtH2SO4] ++ soln 300 wtZn wtH2SO4


-- input wt are the formal concentrations in (g / L)
actRatio noConv_molality wtZn wtHSO4 =
    soln' (\c -> let ?c = c; ?temp = 300 in cs_h ?c * exp lngH / exp lngZn / cs_zn ?c)
        300
        noConv_molality
        wtZn
        wtHSO4

soln temp wtZn wtHSO4 = soln'
        (\c -> let ?c = c; ?temp = temp in
        [ cs_h ?c  * exp lngH,
          cs_1 ?c  * exp lng1,
          cs_2 ?c  * exp lng2,
          cs_zn ?c * exp lngZn,
          exp lngH,
          exp lng1,
          exp lng2,
          exp lngZn ])
    temp False wtZn wtHSO4

-- input wt are the formal concentrations in (g / L)
soln' f temp noConv_molality wtZn wtHSO4 =
    let -- in g / L
        rho = (1153.82 + 66.748 * wtHSO4/98 + 181.436 * (wtZn / 165))
                / 1000

    in let ?temp = temp in -- approximation

    -- initial guess half is SO4=, half is HSO4-
    let initial_molarity =
                  [ wtHSO4 / 98 - wtZn / 165, -- H+
                    0.5 * wtHSO4 / 98,        -- HSO4-
                    0.5 * wtHSO4 / 98 ]       -- SO4=
        toMolality
            | noConv_molality = 1
            | otherwise = rho * (1000 - (wtZn + wtHSO4)) / 1000

        initial = map (/ toMolality) initial_molarity
    in
    let c = let [h,hso4, so4] = fst $
                    root Hybrids
                        1e-10 10 -- convergence criteria
                        (objective (wtZn / 165) (wtHSO4 / 98))
                        initial
            in CS h hso4  so4  (wtZn / 165)
        in
    let ?c = c
    in f c

