{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
module AcidBase.Species (species
) where

import AcidBase.Internal.Collect (collect')
import AcidBase.Internal.Rejoins (rejoins)
import qualified Data.Map as M (fromList, keys)


species = rejoins . collect' . M.fromList . M.keys
