{-# LANGUAGE Rank2Types #-}

{- |

2013 addition: how does this compare with <https://github.com/leftaroundabout/uncertainly-haskell>?

*NumVar> summary $ (5 ± ("b",0.02)) * sin( 5 ± ("a", 0.01) )
-4.794621373315692
    RSD in %
      aggregate   0.49749902612566316
      a           0.2958129155327455
      b           -0.39999999999999997

Is the same as:

    aggregate = sqrt(diff (\x -> x * sin 5 ) 5 ^2 * 0.02^2 + diff (\x -> 5 * sin x) 5 ^ 2 * 0.01^2)
                        / abs(5 * sin 5)


-}
module NumVar where

import Text.XFormat.Show
import Numeric.AD.Mode.Forward
import qualified Data.Map as M

import Text.PrettyPrint
import Data.List

data NV a = NV
      { -- | The expected value
        nv_mean :: !a,
        {- | by sensitivity it is meant:

           > (d (nv_mean m) / d "v")  == (nv_sens m ! "v")

           this is close to a proper definition of sensitivity (of Y with
           respect to x) which is

           >    (dY/dx) * (x/Y)

           which can be interpreted as the percentage change in Y resulting
           from a percentage change in x

        -}
        nv_sens :: ! (M.Map String a),
        {- | variances of uncertain input variables -}
        nv_var :: ! (M.Map String a) }
    deriving (Show, Eq)



summary :: NV Double -> Doc
summary x = hang (double (nv_mean x)) 4
            $ hang (text "RSD in %") 2
                $ vcat $ map (\[a,b] -> a $$ b) (

        [text "aggregate",
         nest gap $ double (100 * sqrt (sum
                                            [ (nv_sens x M.! k)^2 * v
                                                | (k,v) <- M.assocs (nv_var x) ])
                    / abs(nv_mean x))]
        :
        [ [text k ,
            nest gap $ double (100 * nv_sens x M.! k * sqrt v / abs(nv_mean x)) ]
                                        | (k,v) <- M.assocs (nv_var x) ])

        where (<+>) = (Text.PrettyPrint.<+>)
              gap = maximum $ 12: map length (M.keys (nv_var x))

-- a ± (s,n) = NV a (M.singleton s 1) (M.singleton s (n*n))
a ± _ = a

nv :: a -> NV a
nv a = NV a M.empty M.empty

-- * Lifting normal functions to NV
lnv1 :: (Floating b, RealFrac b) => (forall a. (Floating a, RealFrac a) => a -> a) -> NV b -> NV b
lnv1 f x = lnvn (\[a] -> f $! a) [x]

lnv2 :: (Floating b, RealFrac b) => (forall a. (Floating a, RealFrac a) => a -> a -> a) -> NV b -> NV b -> NV b
lnv2 f x y = lnvn (\[a,b] -> f a b) [x,y]

lnvn :: (Floating b, RealFrac b) => (forall a. (Floating a, RealFrac a) => [a] -> a) -> [NV b] -> NV b
lnvn f nvs = case lnvnn (\x -> [f x]) nvs of [a] -> a

lnvnn :: (Floating b, RealFrac b) => (forall a. (Floating a, RealFrac a) => [a] -> [a]) -> [NV b] -> [NV b]
lnvnn f nvs = do
    (fab,dfs) <- f (map nv_mean nvs) `zip` jacobian f (map nv_mean nvs)
    return $ let
       m1 = (foldl' (M.unionWith (+)) M.empty $ zipWith3 (\a dfa -> fmap (\x -> x*dfa ))
                (map nv_mean nvs)
                dfs
                (map nv_sens nvs))
       m2 = (foldl' (M.unionWithKey
            (\k v1 v2 ->
                -- should be an approximate comparison?
                if v1 /= v2 then error $
                        showf ("lnvnn: variance "%String%
                               " duplicated with 2 different values "%
                               Double%Double)
                              k (realToFrac v1) (realToFrac v2)
                else v1))
            M.empty
            (map nv_var nvs))
     in fab `seq` M.fold seq () m1 `seq` M.fold seq () m2 `seq` NV fab m1 m2
instance (Floating a, RealFrac a) => Num (NV a) where
    (+) = lnv2 (+)
    (-) = lnv2 (-)
    (*) = lnv2 (*)
    abs = lnv1 abs
    signum = lnv1 signum
    fromInteger = nv . fromInteger

instance Ord a => Ord (NV a) where
    compare a b = compare (nv_mean a) (nv_mean b)
instance (RealFrac a, Floating a) => Real (NV a) where
    toRational = toRational . nv_mean
instance (RealFrac a, Floating a) => RealFrac (NV a) where
    properFraction (NV a ss nn) = let (c,d) = properFraction a
        in (c, NV d (fmap (\x -> x*a/d) ss) nn)
instance (RealFrac a, Floating a) => Fractional (NV a) where
    fromRational = nv . fromRational

instance (Floating a, RealFrac a) => Floating (NV a) where
  pi = nv pi
  (**) = lnv2 (**)
  logBase = lnv2 logBase
  exp   = lnv1 exp
  sqrt  = lnv1 sqrt
  log   = lnv1 log
  sin   = lnv1 sin
  tan   = lnv1 tan
  cos   = lnv1 cos
  asin  = lnv1 asin
  atan  = lnv1 atan
  acos  = lnv1 acos
  sinh  = lnv1 sinh
  tanh  = lnv1 tanh
  cosh  = lnv1 cosh
  asinh = lnv1 asinh
  atanh = lnv1 atanh
  acosh = lnv1 acosh

