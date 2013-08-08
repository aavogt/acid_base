{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
module AcidBase.Internal.Davies (davies
) where




-- http://www.ocean.washington.edu/courses/oc400/Lecture_Notes/CHPT6.pdf
davies i z1 z2 = exp( - 0.5 * fromIntegral z1 * fromIntegral z2 * ( sqrt i / (1+sqrt i) - 0.2 * i ))
