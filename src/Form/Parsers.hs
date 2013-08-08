{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{- | TODO

* fancy ajax (update on type?)

* ghcjs

* other activity coefficient models

* what prints 7?

-}
module Form.Parsers 
    ( chargeToStr
    , compToStr
    , kAsToStr
    , strToCharge
    , strToComp
    , strTokAs
    ) where

import Control.Applicative ((<$>), liftA2)
import qualified Data.Map as M (fromList, toList)
import Form.Parsers.Internal (compoundName)
import Text.Printf (printf)
import Text.Trifecta (double, integer, many)
import Text.Trifecta hiding (string, string, string, string, string, string)


chargeToStr = unlines . map (\(a,b) -> printf "%s\t%d" a b) . M.toList

compToStr = unlines . map (\(a,b) -> printf "%s\t%.3g" a b) . M.toList


kAsToStr = unlines . map (\((a,b),c) -> printf "%s\t%s\t%.3g" a b (-logBase 10 c)) . M.toList

strToCharge = M.fromList <$> many (liftA2 (,) compoundName integer)


strToComp = M.fromList <$> many (liftA2 (,) compoundName double)


strTokAs = M.fromList <$> many (do
        a <- compoundName
        b <- compoundName
        n <- double
        return ((a,b), 10**(-n)))

