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
module Main 
    ( main
    ) where
import AcidBase.Charge (charge)
import AcidBase.KAs (kAs)
import AcidBase.KaEqs (kaEqs)
import Conf (AcidBase(httpPort), conf0)
import Data.List (transpose)
import qualified Data.Map as M (fromList)
import Form (form0, viewForm)
import Happstack.Server.SimpleHTTP (Conf(port), decodeBody, defaultBodyPolicy, nullConf, ok, simpleHTTP, ToMessage(toResponse))
import Main.Instances ()
import Main.PpKaEq (ppKaEq)
import System.Console.CmdArgs (cmdArgs)
import qualified Text.Blaze.Html5 as H (ToMarkup(toMarkup))
import Text.Digestive.Happstack (runForm)
import Text.Trifecta hiding (string)
import qualified Text.XHtml as X ((<<), p)
import qualified Text.XHtml.Table as X (simpleTable)

main = do
    conf <- cmdArgs conf0
    simpleHTTP nullConf{ port = httpPort conf } $ do
        decodeBody $ defaultBodyPolicy "/tmp/" 32000 1000 1000
        let f = form0 (Just $ M.fromList [("H2O", 55),
                                          ("H2PO4" :: String, 1.15e-3 :: Double),
                                          ("HPO4", 8.06e-3)],
                       Just charge,
                       Just kAs)
        (v, r) <- runForm "" f
        case (v,r) of
            (_, Nothing) -> ok . toResponse $ viewForm v
            (_, Just (a,b,c)) -> ok . toResponse $ do
                viewForm v
                H.toMarkup $ case map (ppKaEq c) $ zipWith (,) [1 .. ] $ kaEqs b c a of
                    [] -> X.p X.<< ("no solution!"::String)
                    xs @ (_:_) -> X.simpleTable [] [] $
                        zipWith (:) (fst (head xs)) $ transpose (map snd xs)
