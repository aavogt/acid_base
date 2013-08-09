{-# LANGUAGE DeriveDataTypeable #-}
module Conf
    ( conf0
    , AcidBase(AcidBase, httpPort)
    ) where
import System.Console.CmdArgs (Data, Typeable)


conf0 = AcidBase { httpPort = 8000 }


data AcidBase = AcidBase { httpPort :: Int } deriving (Show, Data, Typeable)

