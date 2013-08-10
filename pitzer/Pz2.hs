{-# LANGUAGE ImplicitParams, NoMonomorphismRestriction #-}

{- Calculation of activity coefficients using the Pitzer ion interaction
   model for the system involving:

   H+
   HSO4-
   SO4=
   Zn++

  Parameters used are as recommended by (Guerra 2006)
-}
module Pz2 (module Pz2) where

import Numeric.AD.Mode.Forward

a ± b = a -- could be used later for uncertainty propagation

-- single salt pitzer ion interaction coefficients,
-- zc and za are charge of the cation and anion
data Pitzer1 a = Pitzer1 { b0, b1, b2, cphi, zc, za :: a }

pz_zn_so4 = Pitzer1
  { b0 = 0.189,
    b1 = 2.87,
    b2 = -66.467,
    cphi = 0.0329,
    zc = 2,
    za = 2 }
pz_zn_hso4 = Pitzer1
  { b0 = 0.439,
    b1 = 1.811,
    b2 = 0,
    cphi = 0.0397,
    zc = 2,
    za = 1 }
pz_h_so4 = Pitzer1
  { b0 = 0.1131042 ± ("beta0 h2",0.00539),
    b1 = -0.767255 ± ("beta1 h2",0.06223),
    b2 = 0,
    cphi = 0.0076309 ± ("cphi h2",0.00177),
    zc = 1,
    za = 2 }
pz_h_hso4 = Pitzer1
  { b0 = 0.2246349 ± ("beta0 h1",0.00191),
    b1 = 0.7102037 ± ("beta1 h1",0.03290),
    b2 = 0,
    cphi = -0.0040052 ± ("cphi h1",0.00012),
    zc = 1,
    za = 2 }

-- interaction between anions
th12 = -0.0857672 ± ("th12", 0.00238)

-- 3-ion interaction
phi12h = 0.0189687 ± ("phi12h", 0.001)


-- concentrations
data CS a = CS { cs_h, cs_1, cs_2, cs_zn :: a }

-- derivative of ionic strength wrt charge
cs_Z c = cs_h c + cs_1 c + 2*cs_2 c + 2*cs_zn c
-- ionic strength
cs_I c = (cs_h c + cs_1 c + 4*cs_2 c + 4*cs_zn c)/2


-- temperature-dependent Debye-Huckel parameter
aPhi = 0.377 + 0.0004684 * (?temp - 273.15)
        + 0.00000374 * (?temp - 273.15)^2

-- electrostatic
eTH = ((?z1 * ?z2) / 4 / ?im) * (j xij - (j xii + j xjj) / 2 )
    where
        j x = x / (4 + 4.581*x**(-0.7237)*exp(-0.012*x**0.528))
        xij = 6* ?z1* ?z2* aPhi * sqrt(?im)
        xii = 6* ?z1* ?z1* aPhi * sqrt(?im)
        xjj = 6* ?z2* ?z2* aPhi * sqrt(?im)

-- derivative of eTH wrt ionic strength
eTH' = - eTH / ?im + ?z1* ?z2/ (8* ?im^2) * (j' xij - (j' xii + j' xjj) / 2)
    where
        j' x = x * diff j x
        j x = x / (4 + 4.581*x**(-0.7237)*exp(-0.012*x**0.528))
        xij = 6* ?z1* ?z2* aPhi * sqrt(?im)
        xii = 6* ?z1* ?z1* aPhi * sqrt(?im)
        xjj = 6* ?z2* ?z2* aPhi * sqrt(?im)

-- to get better fit according to papers, different alpha
-- are used when both ions are divalent
alpha1 p | abs( za p * zc p ) <= 2 = 2
       | otherwise             = 0

alpha2 p | abs( za p * zc p ) <= 2 = 1.4
       | otherwise             = 12

bmx p = b0 p + b1 p * f (alpha1 p * sqrt ?im)
      + b2 p * f (alpha2 p * sqrt ?im )
    where
        f x | abs x < 1e-30 = 0 -- limit
            | otherwise = 2*( 1 - (1+x)*exp(-x)) / x^2

bmx' p = (b1 p * f' (alpha1 p * sqrt ?im)
            + b2 p * f' (alpha2 p * sqrt ?im)) / ?im
    where
        f' x = exp(-x) - f x
        f x | abs x < 1e-30 = 0 -- limit
            | otherwise = 2*( 1 - (1+x)*exp(-x)) / x^2

-- here it is explicit that some parameters are left out
-- (most theta and phi).
f = - aPhi * ( sqrt ?im / (1 + 1.2 * sqrt ?im)
                + (2/1.2) * log( 1 + 1.2 * sqrt ?im))
    + sum [ cs_zn ?c * cs_2 ?c * bmx' pz_zn_so4,
            cs_zn ?c * cs_1 ?c * bmx' pz_zn_hso4,
            cs_h ?c  * cs_2 ?c * bmx' pz_h_so4,
            cs_h ?c  * cs_1 ?c * bmx' pz_zn_hso4]
    + sum [ let ?z1 = 2; ?z2 = 1 in cs_zn ?c * cs_h ?c * eTH',
            let ?z1 = 2; ?z2 = 1 in cs_2 ?c * cs_1 ?c * eTH' ]

lngZn = let ?z1 = 2; ?im = cs_I ?c  in ?z1 ^ 2 * f
    + sum [ cs_2 ?c * bzc pz_zn_so4,
            cs_1 ?c * bzc pz_zn_hso4 ]
    + sum [ cs_h ?c * let ?z2 = 1 in 2 * eTH,
            cs_zn ?c * let ?z2 = 2 in 2 * eTH
        ] -- assuming theta = 0, phi also 0
    + ?z1 * ca_sum

lngH = let ?z1 = 1; ?im = cs_I ?c  in ?z1 ^ 2 * f
    + sum [ cs_2 ?c * bzc pz_h_so4,
            cs_1 ?c * bzc pz_h_hso4 ]
    + sum [ cs_h ?c * let ?z2 = 1 in 2 * eTH,
            cs_zn ?c * let ?z2 = 2 in 2 * eTH
        ] -- assuming theta = 0, phi also 0
    + ?z1 * ca_sum

lng1 = let ?z1 = -1; ?im = cs_I ?c  in ?z1 ^ 2 * f
    + sum [ cs_h ?c * bzc pz_h_hso4,
            cs_zn ?c * bzc pz_zn_hso4 ]
    + sum [ cs_1 ?c * let ?z2 = -1 in 2 * eTH,
            cs_2 ?c * let ?z2 = -2 in 2 * eTH ]
    + ?z1 * ca_sum

lng2 = let ?z1 = -2; ?im = cs_I ?c  in ?z1 ^ 2 * f
    + sum [ cs_h ?c * bzc pz_h_so4,
            cs_zn ?c * bzc pz_zn_so4 ]
    + sum [ cs_1 ?c * let ?z2 = -1 in 2 * eTH,
            cs_2 ?c * let ?z2 = -2 in 2 * eTH ]
    + ?z1 * ca_sum


bzc p = 2 * bmx p + cs_Z ?c * cphi p / (2 * sqrt( za p * zc p ))
ca_sum = cs_zn ?c * cs_2 ?c * c pz_zn_so4
       + cs_zn ?c * cs_1 ?c * c pz_zn_hso4
       + cs_h ?c  * cs_2 ?c * c pz_h_so4
       + cs_h ?c *  cs_1 ?c * c pz_h_hso4
    where c p = cphi p / (2 * sqrt( za p * zc p ))


