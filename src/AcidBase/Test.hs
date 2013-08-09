{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
module AcidBase.Test (test
) where



import AcidBase.Internal (species)
import qualified Data.Map as M (fromList)
import qualified Data.Sequence as S (fromList)


-- also check about multiple sequences?
test = ssn' "a" 5 == ssn "a" 5
 where
   ssn' a nn = [S.fromList [ a ++ show n | n <- [1 .. (nn+1) :: Double]]]

   ssn a nn = species $ M.fromList [ ((a ++ show n, a ++ show (n+1)), n) | n <- [1 .. nn :: Double] ]

