module AcidBase.Internal.Davies (davies) where 

{- |

@davies i z1 z2@ gives the mean activity coefficient for a compound in solution
(around 25°C) between two ions with charges @z1@ and @z2@

see also  <http://www.ocean.washington.edu/courses/oc400/Lecture_Notes/CHPT6.pdf>
-}
davies i z1 z2 = exp( - 0.5 * fromIntegral z1 * fromIntegral z2 * ( sqrt i / (1+sqrt i) - 0.2 * i ))
