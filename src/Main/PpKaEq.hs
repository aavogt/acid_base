{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Main.PpKaEq
    ( ppKaEq
    ) where
import AcidBase.Species (species)
import qualified Data.Foldable as F (toList)
import qualified Data.Map as M ((\\), fromList, lookup, toList)
import Data.Maybe (fromMaybe, mapMaybe)
import Text.Printf (printf)
import Text.Trifecta hiding (string)
import qualified Text.XHtml as X ((<<), td, th)

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
