{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
module AcidBase.Data (kAs
,charge
) where







import qualified Data.Map as M (fromList, Map)





kAs :: M.Map (String, String) Double
kAs = M.fromList $ [ (("PO4","HPO4"), 2.14e-13), (("HPO4","H2PO4"), 6.2e-8), (("H2PO4","H3PO4"), 7.5e-3),
    (("Ac", "HAc"), 10**(-4.76)),
    (("OH", "H2O"), 10**(-14) / 55)]



charge :: M.Map String Int
charge = M.fromList [ ("PO4", -3) , ("HPO4", -2), ("H2PO4", -1), ("H3PO4", 0),
    ("Ac", -1), ("HAc", 0), ("OH", -1)]

