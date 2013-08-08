{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
module AcidBase.KaEqs (kaEqs
) where

import AcidBase.Internal.KaEqs (kaEqs')
import Control.Monad (guard, when)
import Data.List (unfoldr)

-- kaEqs :: M.Map String Double -> M.Map (String,String) Double -> Matrix Double

kaEqs charge kAs input = unfoldr (\i -> do
        let (r,i') = kaEqs' charge kAs input i
        when (i/=0) $ do
            -- guard (abs(i - i')/1e-3 > 1) -- abs tolerance
            guard ( abs((i - i') / i') > 1e-5 ) -- relative
        return (r,i') ) 0
