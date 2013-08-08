{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
module AcidBase.Charge (charge
) where

import qualified Data.Map as M (fromList, Map)


charge :: M.Map String Int
charge = M.fromList [ ("PO4", -3) , ("HPO4", -2), ("H2PO4", -1), ("H3PO4", 0),
    ("Ac", -1), ("HAc", 0), ("OH", -1)]
