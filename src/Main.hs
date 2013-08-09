{-# LANGUAGE OverloadedStrings #-}
{- | TODO

* fancy ajax (update on type?)

* ghcjs

* other activity coefficient models

* what prints 7?

-}
module Main 
    ( main
    ) where
import AcidBase.Solve (solve, defaultProblem)
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
        let f = form0 defaultProblem
        (v, r) <- runForm "" f
        case (v,r) of
            (_, Nothing) -> ok . toResponse $ viewForm v
            (_, Just problem) -> ok . toResponse $ do
                viewForm v
                H.toMarkup $ case map (ppKaEq problem) (solve problem) of
                    [] -> X.p X.<< ("no solution!"::String)
                    xs @ (_:_) -> X.simpleTable [] [] $
                        zipWith (:) (fst (head xs)) $ transpose (map snd xs)
