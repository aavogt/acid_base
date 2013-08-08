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
module Conf 
    ( conf0
    , AcidBase(AcidBase, httpPort)
    ) where
import System.Console.CmdArgs (Data, Typeable)
import Text.Trifecta hiding (string, string)


conf0 = AcidBase { httpPort = 8000 }


data AcidBase = AcidBase { httpPort :: Int } deriving (Show, Data, Typeable)

