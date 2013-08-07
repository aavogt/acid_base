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
module Main where


import Data.List
import Data.Maybe
import System.Console.CmdArgs ( Data, Typeable, cmdArgs )
import Happstack.Server.SimpleHTTP
    ( ToMessage(toResponse),
      Conf(port),
      simpleHTTP,
      decodeBody,
      ok,
      nullConf,
      defaultBodyPolicy )
import Text.Digestive.Form -- ( (.:), stringRead, listOf, choice )
import Control.Applicative
import Text.Digestive.Happstack ( runForm )
import qualified Data.Text as T ()
import Control.Applicative.QQ.ADo ()
import Text.Digestive.Blaze.Html5
import Text.Digestive.View ( debugViewPaths )
import qualified Text.Blaze.Html5 as H ( br )
import Control.Monad.Trans ( MonadIO(liftIO) )
import Text.Digestive.Form.Internal ( FormTree, debugFormPaths )

import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html4.Transitional.Attributes

import Text.Blaze.Html.Renderer.Utf8

import AcidBase

import qualified Data.Map as M
import Text.Printf

import Text.Trifecta hiding (string)
import Data.Monoid
import qualified Data.Foldable as F

import System.Process
import Text.HTML.TagSoup

import Text.Digestive.Types
import Text.Digestive.View (errors)

import qualified Data.Text as T

import qualified Text.XHtml.Table as X
import qualified Text.XHtml as X
import Data.String

data AcidBase = AcidBase { httpPort :: Int } deriving (Show, Data, Typeable)

conf0 = AcidBase { httpPort = 8000 }

kAsToStr = unlines . map (\((a,b),c) -> printf "%s\t%s\t%.3g" a b (-logBase 10 c)) . M.toList

strTokAs = M.fromList <$> many (do
        a <- compoundName
        b <- compoundName
        n <- double
        return ((a,b), 10**(-n)))

compoundName = some (noneOf " \t\n") <* whiteSpace


test1 = parseString strTokAs mempty $ kAsToStr kAs
test2 = parseString strToCharge mempty $ chargeToStr charge

chargeToStr = unlines . map (\(a,b) -> printf "%s\t%d" a b) . M.toList
strToCharge = M.fromList <$> many (liftA2 (,) compoundName integer)

strToComp = M.fromList <$> many (liftA2 (,) compoundName double)
compToStr = unlines . map (\(a,b) -> printf "%s\t%.3g" a b) . M.toList


parse2 p str = case parseString p mempty str of
    Text.Trifecta.Success a -> return $ Text.Digestive.Types.Success a
    Failure d -> Error . H.preEscapedToMarkup <$> aha (show d)

aha s = renderTags . getBody . parseTags <$> readProcess "aha" [] s
    where
    getBody = takeWhile (/= TagClose "body") . drop 1 . dropWhile (/= TagOpen "body" [])

testAHA = do
    i <- readProcess "aha" ["-h"] ""
    writeFile "foo.html" =<< aha i


parsersToF from to x0 x = validateM (liftIO . parse2 from) (string (fmap (\y -> to (y `asTypeOf` x0)) x))

chargeForm = parsersToF strToCharge chargeToStr charge
kaForm = parsersToF strTokAs kAsToStr kAs
c0Form = parsersToF strToComp compToStr (M.singleton (""::String) (1.0::Double))

form0 (a,b,c) = (,,) <$> "comp" .: c0Form a <*> "charge" .: chargeForm b <*> "eq" .: kaForm c

viewForm v = form v "/" $ do
    H.h1 "Acid-Base equilibrium calculator"
    H.p "Given certain input concentrations, charges and pKas this calculator\
     \ can determine the concentration of all species in the ideal solution at equilibrium.\
     \ Spectator ions, which do not participate in the equilibria are not included\
     \ in the output (and input). However they are considered in the equation for electroneutrality,\
     \ in the sense that the initial concentration H2PO4 is the concentration of some\
     \ spectator cation such as Na+. \
     \ For the equilibrium data, the rows are specified as 'conjugate base' 'acid' pKa.\
     \ The actual names used are unimportant. \
     \ The davies equation is used the outer loop. Only one iteration seems to be necessary\
     \ for values to stabilize (in this case ionic strength varying less than 0.001%)"
    H.table $ do
        H.tr $ do
            H.th "initial composition"
            H.th "ion charges"
            H.th "A HA pKa"

        let w= Just 25
            h= Just 20
        H.tr $ do
            H.td $ do
                childErrorList "comp" v
                inputTextArea h w "comp" v
            H.td $ do
                childErrorList "charge" v
                inputTextArea h w "charge" v
            H.td $ do
                childErrorList "eq" v
                inputTextArea h w "eq" v

    inputSubmit "submit"

main = do
    conf <- cmdArgs conf0
    simpleHTTP nullConf{ port = httpPort conf } $ do
        decodeBody $ defaultBodyPolicy "/tmp/" 32000 1000 1000
        let f = form0 (Just $ M.fromList [("H2PO4" :: String, 1.15e-3 :: Double),
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

instance H.ToMarkup X.Html where
    preEscapedToMarkup = H.preEscapedToMarkup . show
    toMarkup = H.preEscapedToMarkup . show

ppKaEq c (n, (s', extras)) =
    let (sol, solMap, (gamma11,i0)) =
            let sp = species c
                sorted = mapMaybe (\x -> fmap (x,) $ x `M.lookup` s')
                        $ concatMap F.toList sp
                missing = s' M.\\ M.fromList sorted
            in (M.toList missing ++ sorted, s', extras)
    in let
        rowNames =
            (X.th X.<< ("iteration"::String))  :
            map ( (X.th X.<<) . fst) sol
            ++
            map (X.th X.<<) ["gamma11", "pH", "ionic strength" :: String]
        rowContent = map (X.td X.<<)
                $ show n: map (printf "%.3g") (map snd sol
                        ++ [gamma11 :: Double,
                            -logBase 10 (fromMaybe 0 $ M.lookup "H" solMap),
                            i0])

    in (rowNames, rowContent)
