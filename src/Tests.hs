{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Tests 
    ( test1
    , test2
    , testAHA
    ) where

import AcidBase.Charge (charge)
import AcidBase.KAs (kAs)
import Data.Monoid (Monoid(mempty))
import Form.Parsers (chargeToStr, kAsToStr, strToCharge, strTokAs)
import Main.Aha (aha)
import System.Process (readProcess)
import Text.Trifecta (parseString)
import Text.Trifecta hiding (string, string, string)



test1 = parseString strTokAs mempty $ kAsToStr kAs

test2 = parseString strToCharge mempty $ chargeToStr charge


testAHA = do
    i <- readProcess "aha" ["-h"] ""
    writeFile "foo.html" =<< aha i

